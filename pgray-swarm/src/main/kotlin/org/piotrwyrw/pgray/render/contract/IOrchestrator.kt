/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.render.contract

import org.piotrwyrw.pgray.db.DatabaseManager
import org.piotrwyrw.pgray.docker.DockerManager
import org.piotrwyrw.pgray.render.Tile
import org.piotrwyrw.pgray.render.Worker

interface IOrchestrator {
    fun getDockerManager(): DockerManager
    fun getDatabaseManager(): DatabaseManager

    fun startInspectionThread()

    fun subscribe(listener: IOrchestratorListener)
    fun getTiles(): List<Tile>
    fun createRenderingTiles(imageWidth: Int, imageHeight: Int, subdivisions: Int)

    fun createAllWorkers()
    fun removeAllWorkers()
    fun startAllWorkers()
    fun stopAllWorkers()
    fun inspectAllWorkers()

    fun createWorker(tile: Tile)
    fun removeWorker(worker: Worker)
    fun startWorker(worker: Worker)
    fun stopWorker(worker: Worker)
    fun inspectWorker(worker: Worker)

    fun awaitCompletion();

    fun getWorkerOfTile(tile: Tile): Worker?
}