/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.window

import com.formdev.flatlaf.util.SystemInfo
import javax.swing.JFrame
import javax.swing.JRootPane

open class BaseWindow(
    _title: String
) : JFrame(_title) {
    companion object {
        fun applyMacOsFeatures(rootPane: JRootPane) {
            if (SystemInfo.isMacFullWindowContentSupported) {
                rootPane.putClientProperty("apple.awt.fullWindowContent", true)
                rootPane.putClientProperty("apple.awt.transparentTitleBar", true)
            }
        }
    }

    init {
        applyMacOsFeatures(rootPane)
    }
}