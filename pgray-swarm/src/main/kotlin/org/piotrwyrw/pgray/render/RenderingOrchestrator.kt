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
    fun startWorker(worker: Worker)
    fun stopWorker(worker: Worker)
    fun awaitCompletion(timeoutMs: Long);
}