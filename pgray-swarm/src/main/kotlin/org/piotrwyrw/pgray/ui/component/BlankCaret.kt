/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.component

import java.awt.Graphics
import javax.swing.text.DefaultCaret

class BlankCaret : DefaultCaret() {
    override fun paint(g: Graphics) = Unit
}