package org.piotrwyrw.pgray.ui

import org.piotrwyrw.pgray.RenderingOrchestrator
import java.awt.BasicStroke
import java.awt.Color
import java.awt.Graphics
import java.awt.Graphics2D
import javax.swing.JPanel
import kotlin.math.roundToInt

class Viewport(val orchestrator: RenderingOrchestrator) : JPanel() {

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
        revalidate()
        repaint()
    }

    override fun paintComponent(g: Graphics) {
        g.color = Color.BLACK
        g.fillRect(0, 0, width, height)

        orchestrator.tiles.forEach { tile ->
            val fromX = (tile.fromX.toDouble() / imageWidth) * width
            val fromY = (tile.fromY.toDouble() / imageHeight) * height

            val toX = (tile.toX.toDouble() / imageWidth) * width
            val toY = (tile.toY.toDouble() / imageHeight) * height

            val tileWidth = toX - fromX
            val tileHeight = toY - fromY

            val ax = fromX.roundToInt()
            val ay = fromY.roundToInt()
            val w = tileWidth.roundToInt()
            val h = tileHeight.roundToInt()

            g.color = tile.viewportColor
            g.fillRect(fromX.roundToInt(), fromY.roundToInt(), tileWidth.roundToInt(), tileHeight.roundToInt())

            g.color = Color.black
            g.drawRect(ax, ay, w, h)
        }
    }
}