/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.frame

import java.awt.Dimension
import java.awt.GridBagLayout
import java.awt.LayoutManager
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.JFrame

abstract class BaseFrame(
    frameTitle: String,
    val initialWidth: Int,
    val initialHeight: Int,
    val _resizable: Boolean = false,
    val closeOperator: Int = EXIT_ON_CLOSE,
    val layoutManager: LayoutManager = GridBagLayout(),
    val centered: Boolean = true,
    val showAfterCreation: Boolean = true
) : JFrame(frameTitle) {

    fun create(): BaseFrame {
        size = Dimension(initialWidth, initialHeight)
        defaultCloseOperation = closeOperator
        layout = layoutManager
        isResizable = _resizable

        buildUI()

        addWindowListener(object : WindowAdapter() {
            override fun windowClosing(e: WindowEvent?) {
                onWindowClosing()
            }
        })

        if (centered) setLocationRelativeTo(null)

        if (showAfterCreation)
            showFrame()

        return this
    }

    abstract fun buildUI()
    open fun onWindowClosing() = Unit
    open fun afterFrameShown() = Unit

    fun showFrame() {
        isVisible = true
        afterFrameShown()
    }

    fun hideFrame() {
        isVisible = false
    }

    fun createAndShow() {
        create()
        isVisible = true
    }
}