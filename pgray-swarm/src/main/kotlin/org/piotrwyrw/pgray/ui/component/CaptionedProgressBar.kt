/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.component

import org.piotrwyrw.pgray.ui.fillHorizontal
import org.piotrwyrw.pgray.ui.gbc
import org.piotrwyrw.pgray.ui.smInsets
import org.piotrwyrw.pgray.ui.theming.Theme
import java.awt.Graphics
import java.awt.GridBagLayout
import javax.swing.JLabel
import javax.swing.JPanel
import kotlin.math.roundToInt

class CaptionedProgressBar(
    initialProgress: Float = 0f,
    initialCaption: String = "",
    val onComplete: () -> Unit = {}
) : JPanel() {

    private var _progress: Float = initialProgress.coerceIn(0f..100f)

    var progress: Float
        get() = _progress
        set(newProgress) {
            val clamped = newProgress.coerceIn(0f..100f)

            if (_progress == clamped) return

            val previous = _progress
            _progress = clamped

            repaint()

            if (previous < 100f && _progress >= 100f) onComplete()
        }

    val roundedProgress get() = progress.roundToInt()

    var caption: String
        get() = captionLabel.text
        set(newCaption) {
            captionLabel.text = newCaption
        }

    private val captionLabel = JLabel(initialCaption).apply {
        foreground = Theme.Text.foreground
    }

    init {
        layout = GridBagLayout()

        gbc(0, 0).fillHorizontal.smInsets.let { gbc ->
            add(captionLabel, gbc)
        }
    }

    fun update(caption: String, progress: Float) {
        this.progress = progress
        this.caption = caption
    }

    override fun paintComponent(g: Graphics) {
        super.paintComponent(g)

        val progressWidth = ((progress / 100f) * width).toInt().coerceIn(0, width)
        g.color = foreground
        g.fillRect(0, 0, progressWidth, height)
    }
}