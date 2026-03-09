/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.render.contract

import org.piotrwyrw.pgray.render.Tile
import org.piotrwyrw.pgray.render.Worker

@FunctionalInterface
interface IOrchestratorListener {
    fun onRenderingTilesCreated(imageWidth: Int, imageHeight: Int, tiles: List<Tile>) = Unit
    fun onRenderingTilesCleared() = Unit

    fun onWorkerCreated(worker: Worker) = Unit
    fun onWorkerRemoved(worker: Worker) = Unit

    fun onWorkerStatusRetrieved(worker: Worker) = Unit

    fun onWorkerStarted(worker: Worker) = Unit
    fun onWorkerStopped(worker: Worker) = Unit
}