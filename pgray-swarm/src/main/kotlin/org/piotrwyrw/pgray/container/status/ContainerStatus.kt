/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.container.status

import org.piotrwyrw.pgray.ui.theming.Theme
import java.awt.Color

enum class ContainerStatus {
    ABSENT,
    STOPPED,
    RUNNING;

    fun color(): Color {
        val statusColors = Theme.containerStatus
        return when (this) {
            ABSENT -> statusColors.absent
            STOPPED -> statusColors.stopped
            RUNNING -> statusColors.running
        }
    }
}