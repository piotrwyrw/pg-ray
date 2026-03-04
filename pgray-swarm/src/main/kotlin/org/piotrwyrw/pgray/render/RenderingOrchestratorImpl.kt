package org.piotrwyrw.pgray.render

import org.piotrwyrw.pgray.Tile
import org.piotrwyrw.pgray.Worker
import org.piotrwyrw.pgray.db.DatabaseManager
import org.piotrwyrw.pgray.docker.DockerManager
import org.piotrwyrw.pgray.render.listener.RenderingOrchestratorListener
import org.piotrwyrw.pgray.tileColor
import org.slf4j.LoggerFactory
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import javax.swing.SwingUtilities

class RenderingOrchestratorImpl : RenderingOrchestrator {

    private val logger = LoggerFactory.getLogger(javaClass)

    private val dockerManager: DockerManager = DockerManager()
    private val databaseManager: DatabaseManager = DatabaseManager()

    private val tiles = mutableListOf<Tile>()
    private val workers = hashMapOf<String, Worker>()
    private val listeners = mutableListOf<RenderingOrchestratorListener>()

    private val asyncExecutor: ExecutorService = Executors.newCachedThreadPool()

    init {
        startHousekeepingThread()
    }

    fun <T> submitAsync(call: () -> T, success: (T) -> Unit, error: (Throwable) -> Unit) {
        asyncExecutor.submit {
            try {
                val result = call()
                success(result)
            } catch (e: Exception) {
                error(e)
            }
        }
    }

    private fun startHousekeepingThread() {
        logger.info("Started housekeeping thread")
        Executors.newSingleThreadScheduledExecutor()
            .scheduleAtFixedRate({
                inspectWorkerStatus()
            }, 0, 500, TimeUnit.MILLISECONDS)
    }

    override fun subscribe(listener: RenderingOrchestratorListener) {
        listeners += listener
    }

    fun notify(invocations: RenderingOrchestratorListener.() -> Unit) {
        listeners.forEach { listener ->
            SwingUtilities.invokeLater {
                listener.invocations()
            }
        }
    }

    private fun createWorker(tileNumber: Int) {
        val container = dockerManager.createPostgresContainer()

        val worker = Worker(tileNumber, container)
        workers[container.containerId] = worker

        notify { onWorkerCreated(worker) }
    }

    private fun clearTiles() {
        tiles.clear()

        notify { onRenderingTilesCleared() }
    }

    override fun getTiles(): List<Tile> = tiles

    override fun createRenderingTiles(imageWidth: Int, imageHeight: Int, subdivisions: Int) {
        clearTiles()

        val count = subdivisions * subdivisions
        val tileWidth = imageWidth.toDouble() / subdivisions
        val tileHeight = imageHeight.toDouble() / subdivisions

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

                tiles.add(Tile(fromX.toInt(), fromY.toInt(), toX.toInt(), toY.toInt(), tileColor(x * y, count)))
            }
        }

        notify { onRenderingTilesCreated(imageWidth, imageHeight, tiles) }
    }

    fun inspectWorkerStatus() {
        workers.forEach { id, _ ->
            submitAsync({
                dockerManager.isContainerRunning(id)
            }, { status ->
                val worker = workers[id]!!
                synchronized(workers) {
                    workers[id]?.status = status
                    if (status == WorkerStatus.NOT_EXIST)
                        workers.remove(id)
                }
                notify { onWorkerStatusRetrieved(worker) }
            }, { e ->
                logger.warn("Could not inspect container $id. Assuming it is gone.")
                synchronized(workers) {
                    workers.remove(id)
                }
            })
        }
    }

    override fun createAllWorkers() {
        tiles.forEachIndexed { index, tile ->
            createWorker(index + 1)
        }
    }

    override fun removeAllWorkers() {
        workers.forEach { id, worker ->
            submitAsync({
                dockerManager.removeContainer(id)
            }, { _ ->
                notify { onWorkerRemoved(worker) }
            }, { _ -> })
        }
    }

    override fun startAllWorkers() {
        workers.forEach { id, worker ->
            submitAsync({
                dockerManager.startContainer(id)
            }, { _ ->
                notify { onWorkerStarted(worker) }
            }, { _ -> })
        }
    }

    override fun stopAllWorkers() {
        workers.forEach { id, worker ->
            submitAsync({
                dockerManager.stopContainer(id)
            }, { _ ->
                notify { onWorkerStopped(worker) }
            }, { _ -> })
        }
    }

    override fun startWorker(worker: Worker) {
        dockerManager.startContainer(worker.container.containerId)
    }

    override fun stopWorker(worker: Worker) {
        dockerManager.stopContainer(worker.container.containerId)
    }

    override fun awaitCompletion(timeoutMs: Long) {
        asyncExecutor.awaitTermination(timeoutMs, TimeUnit.MILLISECONDS)
    }
}