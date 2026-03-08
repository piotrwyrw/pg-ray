/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.listener

import java.awt.event.WindowEvent
import java.awt.event.WindowListener

interface SimplifiedWindowListener : WindowListener {
    override fun windowOpened(e: WindowEvent) = Unit
    override fun windowClosing(e: WindowEvent) = Unit
    override fun windowClosed(e: WindowEvent) = Unit
    override fun windowIconified(e: WindowEvent) = Unit
    override fun windowDeiconified(e: WindowEvent) = Unit
    override fun windowActivated(e: WindowEvent) = Unit
    override fun windowDeactivated(e: WindowEvent) = Unit
}