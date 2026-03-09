/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray

import org.piotrwyrw.pgray.render.RenderingOrchestratorImpl
import org.piotrwyrw.pgray.ui.frame.swarm.SwarmFrame
import org.piotrwyrw.pgray.ui.frame.dialog.DialogOption
import org.piotrwyrw.pgray.ui.frame.dialog.DialogType
import org.piotrwyrw.pgray.ui.frame.dialog.MessageDialogFrame
import org.piotrwyrw.pgray.ui.theming.ThemeMode
import org.piotrwyrw.pgray.ui.theming.useTheme

fun main() {
    val orchestrator = RenderingOrchestratorImpl()
    useTheme(ThemeMode.DARK) {
        val frame = SwarmFrame(orchestrator).create()
    }
}