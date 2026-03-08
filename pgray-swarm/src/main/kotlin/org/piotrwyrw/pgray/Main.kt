/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray

import org.piotrwyrw.pgray.render.RenderingOrchestratorImpl
import org.piotrwyrw.pgray.ui.SwarmFrame
import org.piotrwyrw.pgray.ui.theming.ThemeMode
import org.piotrwyrw.pgray.ui.theming.useTheme

fun main() {
    val orchestrator = RenderingOrchestratorImpl()
    useTheme(ThemeMode.DARK) {
        SwarmFrame(orchestrator)
    }
}