/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.dialog

import org.piotrwyrw.pgray.ui.window.dialog.MessageDialogWindow
import javax.swing.JFrame

object MessageDialog {
    fun show(type: DialogType, title: String, message: String, parent: JFrame, vararg options: DialogOption) {
        MessageDialogWindow(
            parent,
            title,
            message,
            type,
            options.toMutableList()
        ).create()
    }
}