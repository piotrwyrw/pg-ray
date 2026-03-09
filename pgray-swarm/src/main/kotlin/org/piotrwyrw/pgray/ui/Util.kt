/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui

import java.awt.GridBagConstraints
import java.awt.Insets
import javax.swing.JComponent
import javax.swing.JPanel

fun gbc(x: Int = 0, y: Int = 0, config: GridBagConstraints.() -> Unit = {}): GridBagConstraints {
    val c = GridBagConstraints()
    c.gridx = x
    c.gridy = y
    c.config()
    return c
}

fun Insets(inset: Int) = Insets(inset, inset, inset, inset)

fun Insets(top: Int, left: Int, bottom: Int, right: Int) = Insets(top, left, bottom, right)

fun Insets(block: Insets.() -> Unit): Insets = Insets(0, 0, 0, 0).apply(block)

val GridBagConstraints.smInsets: GridBagConstraints
    get() = apply {
        insets = Insets(5)
    }

val GridBagConstraints.mdInsets: GridBagConstraints
    get() = apply {
        insets = Insets(10)
    }

val GridBagConstraints.lgInsets: GridBagConstraints
    get() = apply {
        insets = Insets(15)
    }

val GridBagConstraints.xlInsets: GridBagConstraints
    get() = apply {
        insets = Insets(20)
    }

val GridBagConstraints.xxlInsets: GridBagConstraints
    get() = apply {
        insets = Insets(40)
    }

fun GridBagConstraints.insets(inset: Int) = apply {
    this.insets = Insets(inset)
}

val GridBagConstraints.fillHorizontal: GridBagConstraints
    get() = apply {
        weightx = 1.0
        fill = GridBagConstraints.HORIZONTAL
    }

val GridBagConstraints.fillVertical: GridBagConstraints
    get() = apply {
        weighty = 1.0
        fill = GridBagConstraints.VERTICAL
    }

val GridBagConstraints.fillBoth: GridBagConstraints
    get() = apply {
        weightx = 1.0
        weighty = 1.0
        fill = GridBagConstraints.BOTH
    }

fun JComponent.placeholderPanel(x: Int = 0, y: Int = 0) = gbc(x, y).fillBoth.let { gbc ->
    add(JPanel().apply {
        isOpaque = false
    }, gbc)
}