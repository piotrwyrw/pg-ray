/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.dialog

import org.piotrwyrw.pgray.ui.window.dialog.DialogActionFn

data class DialogOption(
    var label: String = "",
    var highlighted: Boolean = false,
    var action: DialogActionFn = { _ -> }
) {
    companion object {
        fun defaultOption() = DialogOption("OK", true) { dialog ->
            dialog.dispose()
        }
    }

    fun label(label: () -> String) {
        this.label = label()
    }

    fun highlight(highlighted: () -> Boolean) {
        this.highlighted = highlighted()
    }

    fun onClick(action: DialogActionFn) {
        this.action = action
    }
}

fun option(block: DialogOption.() -> Unit): DialogOption {
    return DialogOption().apply(block)
}