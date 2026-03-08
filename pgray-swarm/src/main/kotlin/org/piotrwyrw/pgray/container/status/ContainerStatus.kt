/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.container.status

import org.piotrwyrw.pgray.ui.theming.ThemeColors
import java.awt.Color

enum class ContainerStatus {
    ABSENT,
    STOPPED,
    RUNNING;

    fun color(): Color {
        val statusColors = ThemeColors.containerStatus
        return when (this) {
            ABSENT -> statusColors.CONTAINER_ABSENT
            STOPPED -> statusColors.CONTAINER_STOPPED
            RUNNING -> statusColors.CONTAINER_RUNNING
        }
    }
}