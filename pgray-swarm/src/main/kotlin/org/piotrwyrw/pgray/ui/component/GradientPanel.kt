/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.component

import java.awt.Color
import java.awt.GradientPaint
import java.awt.Graphics
import java.awt.Graphics2D
import javax.swing.JPanel

class GradientPanel(
    val direction: GradientDirection,
    var from: Color,
    var to: Color
) : JPanel() {

    companion object {
        val TRANSPARENT_COLOR = Color(0, 0, 0, 0)
    }

    enum class GradientDirection {
        TOP_DONW,
        LEFT_RIGHT,
        RIGHT_LEFT
    }

    private var computedGradientOffset: (JPanel) -> Float = { 0f }

    fun computedOffset(fn: (JPanel) -> Float) {
        computedGradientOffset = fn
    }

    private fun computeOffset() = computedGradientOffset(this)

    fun GradientDirection.gradientPaint(): GradientPaint {
        when (this) {
            GradientDirection.TOP_DONW -> return GradientPaint(
                0f,
                computeOffset(),
                from,
                0f,
                height.toFloat(),
                to
            )

            GradientDirection.LEFT_RIGHT -> return GradientPaint(
                computeOffset(),
                0f,
                from,
                width.toFloat(),
                0f,
                to
            )

            GradientDirection.RIGHT_LEFT -> return GradientPaint(
                width.toFloat() - computeOffset(),
                0f,
                from,
                0f,
                0f,
                to
            )
        }
    }

    override fun paintComponent(g: Graphics) {
        super.paintComponent(g)
        val g2d = g as Graphics2D

        g2d.paint = direction.gradientPaint()
        g2d.fillRect(0, 0, width, height)
    }
}