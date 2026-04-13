/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.render

import com.github.dockerjava.api.exception.InternalServerErrorException
import com.github.dockerjava.api.exception.NotFoundException
import org.piotrwyrw.pgray.db.DatabaseManager
import org.piotrwyrw.pgray.docker.DockerManager
import org.piotrwyrw.pgray.docker.status.ContainerStatus
import org.piotrwyrw.pgray.render.contract.IOrchestrator
import org.piotrwyrw.pgray.render.contract.IOrchestratorListener
import org.piotrwyrw.pgray.scheduling.Priority
import org.piotrwyrw.pgray.scheduling.PriorityRateLimitingScheduler
import org.slf4j.LoggerFactory
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import javax.swing.SwingUtilities

class RenderingOrchestratorImpl : IOrchestrator {

    private val logger = LoggerFactory.getLogger(javaClass)

    private val dockerManager: DockerManager = DockerManager()
    private val dbManager: DatabaseManager = DatabaseManager()

    private val tiles = mutableListOf<Tile>()
    private val workers = hashMapOf<String, Worker>()
    private val listeners = mutableListOf<IOrchestratorListener>()

    private val scheduler = PriorityRateLimitingScheduler()

    override fun getDockerManager(): DockerManager = dockerManager

    override fun getDatabaseManager(): DatabaseManager = dbManager

    override fun startInspectionThread() {
        logger.info("Started inspection thread")
        Executors.newSingleThreadScheduledExecutor()
            .scheduleAtFixedRate({
                inspectAllWorkers()
            }, 0, 1, TimeUnit.SECONDS)
    }

    override fun subscribe(listener: IOrchestratorListener) {
        listeners += listener
    }

    fun notify(invocations: IOrchestratorListener.() -> Unit) {
        listeners.forEach { listener ->
            SwingUtilities.invokeLater {
                listener.invocations()
            }
        }
    }

    private fun clearTiles() {
        tiles.clear()

        notify { onRenderingTilesCleared() }
    }

    override fun getTiles(): List<Tile> = tiles

    override fun createRenderingTiles(imageWidth: Int, imageHeight: Int, subdivisions: Int) {
        clearTiles()

        val tileWidth = imageWidth.toDouble() / subdivisions
        val tileHeight = imageHeight.toDouble() / subdivisions

        var tileNumber: Int = 1

        for (x in 0 until subdivisions) {
            for (y in 0 until subdivisions) {
                val fromX = x * tileWidth
                val fromY = y * tileHeight

                var toX = (x + 1) * tileWidth
                var toY = (y + 1) * tileHeight

                // Rightmost tile
                if (x == subdivisions - 1) {
                    toX = imageWidth.toDouble()
                }

                // Bottom tile
                if (y == subdivisions - 1) {
                    toY = imageHeight.toDouble()
                }

                tiles.add(
                    Tile(
                        tileNumber++,
                        fromX.toInt(),
                        fromY.toInt(),
                        toX.toInt(),
                        toY.toInt()
                    )
                )
            }
        }

        notify { onRenderingTilesCreated(imageWidth, imageHeight, tiles) }
    }

    override fun inspectAllWorkers() {
        workers.forEach { (_, worker) ->
            inspectWorker(worker)
        }
    }

    override fun createAllWorkers() {
        tiles.forEachIndexed { index, tile ->
            createWorker(tile)
        }
    }

    override fun removeAllWorkers() {
        workers.forEach { (_, worker) ->
            removeWorker(worker)
        }
    }

    override fun startAllWorkers() {
        workers.forEach { (_, worker) ->
            startWorker(worker)
        }
    }

    override fun stopAllWorkers() {
        workers.forEach { (_, worker) ->
            stopWorker(worker)
        }
    }

    override fun createWorker(tile: Tile) {
        scheduler.submit({
            dockerManager.createPostgresContainer()
        }, { container, _ ->
            val worker = Worker(tile, container)

            synchronized(workers) {
                workers[container.containerId] = worker
            }

            logger.debug("Created worker for tile ${tile.tileNumber}: ${worker.container.containerId}")

            inspectWorker(worker)

            notify { onWorkerCreated(worker) }
        }, { t, retry ->
            logger.warn(
                "Failed to create worker for tile ${tile.tileNumber}. Trying again. Failure cause: ${
                    t.javaClass.simpleName
                }: ${t.message}"
            )
            retry()
        }, Priority.WORKER_CREATE)
    }

    override fun startWorker(worker: Worker) {
        scheduler.submit({
            dockerManager.startContainer(worker.container.containerId)
        }, { _, _ ->
            notify { onWorkerStarted(worker) }
        }, { t, retry ->
            if (t is InternalServerErrorException && t.message?.lowercase()
                    ?.contains("port\\s+is\\s+already\\s+allocated".toRegex()) ?: false
            ) {
                val newPort = dockerManager.allocNextAvailablePort()
                logger.warn(
                    "Failed to start worker ${worker.container.containerId}: Port (${
                        worker.container.port
                    }) is already allocated. Trying again with port ($newPort)"
                )
                retry()
                return@submit
            }

            logger.warn(
                "Failed to start worker ${worker.container.containerId}. Trying again. Failure cause: ${
                    t.javaClass.simpleName
                }: ${t.message}"
            )
            retry()
        }, Priority.WORKER_START)
    }

    override fun stopWorker(worker: Worker) {
        scheduler.submit({
            dockerManager.stopContainer(worker.container.containerId)
        }, { _, _ ->
            notify { onWorkerStopped(worker) }
        }, { t, retry ->
            if (t is NotFoundException) {
                logger.warn("Attempted to remove container which doesn't exist: ${worker.container.containerId}")
                workers.remove(worker.container.containerId)
                notify { onWorkerRemoved(worker.copy()) }
                return@submit
            }
            logger.warn(
                "Failed to stop worker ${worker.container.containerId}. Trying again. Failure cause: ${
                    t.javaClass.simpleName
                }: ${t.message}"
            )
            retry()
        }, Priority.WORKER_STOP)
    }

    override fun removeWorker(worker: Worker) {
        scheduler.submit({
            dockerManager.removeContainer(worker.container.containerId)
            synchronized(workers) {
                workers.remove(worker.container.containerId)
            }
        }, { _, _ ->
            notify { onWorkerRemoved(worker) }
        }, { t, retry ->
            logger.warn(
                "Could not remove container ${worker.container.containerId}. Trying again. Failure cause:: ${
                    t.javaClass.simpleName
                }: ${t.message?.trim()} "
            )
            retry()
        }, Priority.WORKER_REMOVE)
    }

    override fun inspectWorker(worker: Worker) {
        val containerId = worker.container.containerId

        scheduler.submit({
            dockerManager.inspectContainer(containerId)
        }, { status, _ ->

            logger.debug(
                "Inspecting worker for tile ${worker.tile.tileNumber}: ${
                    status.containerStatus.toString().uppercase()
                }, ${status.healthStatus.toString().uppercase()}"
            )

            val removedWorker: Worker?
            val updatedWorker: Worker?

            synchronized(workers) {
                val worker = workers[containerId] ?: return@submit
                worker.status = status

                if (status.containerStatus == ContainerStatus.ABSENT) {
                    removedWorker = worker.copy()
                    updatedWorker = worker.copy()
                    workers.remove(containerId)
                } else {
                    removedWorker = null
                    updatedWorker = worker
                }
            }

            removedWorker?.let { notify { onWorkerRemoved(it) } }
            updatedWorker?.let { notify { onWorkerStatusRetrieved(it) } }
        }, { error, retry ->
            if (error is NotFoundException) {
                logger.info("Container $containerId no longer present. Removing it from the worker list")
                synchronized(workers) {
                    workers.remove(containerId)
                }
                return@submit
            }

            logger.warn(
                "Could not inspect container $containerId. Trying again. Failure cause: ${
                    error.javaClass.simpleName
                }: ${error.message}"
            )

            retry()
        }, Priority.WORKER_INSPECT)
    }

    override fun awaitCompletion() {
        scheduler.shutdown()
    }

    override fun getWorkerOfTile(tile: Tile): Worker? = synchronized(workers) {
        workers.values.find { it.tile == tile }
    }
}