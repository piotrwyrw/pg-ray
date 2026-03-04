package org.piotrwyrw.pgray.render.listener

import org.piotrwyrw.pgray.Tile
import org.piotrwyrw.pgray.Worker

@FunctionalInterface
interface RenderingOrchestratorListener {
    fun onRenderingTilesCreated(imageWidth: Int, imageHeight: Int, tiles: List<Tile>)
    fun onRenderingTilesCleared()

    fun onWorkerCreated(worker: Worker)
    fun onWorkerRemoved(worker: Worker);

    fun onWorkerStatusRetrieved(worker: Worker)

    fun onWorkerStarted(worker: Worker)
    fun onWorkerStopped(worker: Worker)
}