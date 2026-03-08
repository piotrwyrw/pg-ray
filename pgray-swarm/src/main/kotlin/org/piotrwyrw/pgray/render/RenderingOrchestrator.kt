/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.render

import org.piotrwyrw.pgray.Tile
import org.piotrwyrw.pgray.Worker
import org.piotrwyrw.pgray.render.listener.RenderingOrchestratorListener

interface RenderingOrchestrator {
    fun subscribe(listener: RenderingOrchestratorListener)
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