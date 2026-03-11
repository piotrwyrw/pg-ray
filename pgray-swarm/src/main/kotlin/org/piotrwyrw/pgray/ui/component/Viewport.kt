/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.component

import org.piotrwyrw.pgray.docker.status.ContainerHealthStatus
import org.piotrwyrw.pgray.render.contract.IOrchestrator
import org.piotrwyrw.pgray.ui.theming.Theme
import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import javax.swing.JPanel
import kotlin.math.floor

class Viewport(val orchestrator: IOrchestrator) : JPanel() {
    private var imageWidth: Int = 0
    private var imageHeight: Int = 0
    private var aspect: Double = 0.0

    override fun setBounds(x: Int, y: Int, width: Int, height: Int) {
        val maxWidth = parent?.width ?: 100
        val maxHeight = parent?.height ?: 100

        var width = maxWidth
        var height = (width / aspect).toInt()

        if (height > maxHeight) {
            height = maxHeight
            width = (height * aspect).toInt()
        }

        super.setBounds(
            parent?.width?.let { w -> w / 2 - width / 2 } ?: x,
            parent?.height?.let { h -> h / 2 - height / 2 } ?: y,
            width,
            height
        )
    }

    fun updateAspect(width: Int, height: Int) {
        if (width == 0 || height == 0) return

        imageWidth = width
        imageHeight = height

        aspect = width.toDouble() / height.toDouble()
    }

    private fun Graphics.drawCenteredString(str: String, x: Int, y: Int, width: Int, height: Int) {
        font = font.deriveFont((height * 0.2).toFloat())
        val strWidth = fontMetrics.stringWidth(str)
        val drawX = x + width / 2 - strWidth / 2
        val drawY = y + height / 2 + fontMetrics.ascent / 2
        drawString(str, drawX, drawY)
    }

    override fun paintComponent(g: Graphics) {
        val g2 = g as Graphics2D;

        g.color = Color.BLACK
        g.fillRect(0, 0, width, height)

        val tileCount = orchestrator.getTiles().size

        orchestrator.getTiles().forEach { tile ->
            val tileWorker = orchestrator.getWorkerOfTile(tile)
            val healthStatus = tileWorker?.status?.healthStatus ?: ContainerHealthStatus.UNDEFINED
            val healthColor = healthStatus.color()

            val fromX = (tile.fromX.toDouble() / imageWidth) * width
            val fromY = (tile.fromY.toDouble() / imageHeight) * height

            val toX = (tile.toX.toDouble() / imageWidth) * width
            val toY = (tile.toY.toDouble() / imageHeight) * height

            val tileWidth = floor(toX - fromX)
            val tileHeight = floor(toY - fromY)

            val ax = floor(fromX).toInt()
            val ay = floor(fromY).toInt()
            val w = floor(tileWidth).toInt()
            val h = floor(tileHeight).toInt()

            g.color = healthColor
            g.fillRect(ax, ay, w, h)

            g.color = Theme.accent.accent2
            g.drawRect(ax - 1, ay - 1, w + 1, h + 1)

            // Tile number indicator
            g.color = Theme.text.foreground
            g.drawCenteredString(
                tile.tileNumber.toString(),
                fromX.toInt(),
                fromY.toInt(),
                tileWidth.toInt(),
                tileHeight.toInt()
            )
        }

        if (tileCount > 0) {
            g.color = Theme.surface.layer9
            g.drawLine(0, 0, width, 0)
        }

    }
}