/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.frame.dialog

data class DialogOption(
    var label: String = "",
    var highlighted: Boolean = false,
    var action: DialogActionFn = { _ -> }
) {
    companion object {
        fun default() = DialogOption("OK", true) { dialog ->
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