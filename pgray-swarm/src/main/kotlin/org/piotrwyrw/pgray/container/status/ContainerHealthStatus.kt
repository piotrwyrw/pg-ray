/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.container.status

import org.piotrwyrw.pgray.ui.theming.ThemeColors
import java.awt.Color

enum class ContainerHealthStatus {
    UNDEFINED,
    STARTING,
    HEALTHY,
    UNHEALTHY;

    fun color(): Color {
        val healthColors = ThemeColors.containerHealth
        return when (this) {
            UNDEFINED -> healthColors.UNDEFINED
            STARTING -> healthColors.STARTING
            HEALTHY -> healthColors.HEALTHY
            UNHEALTHY -> healthColors.UNHEALTHY
        }
    }
}

fun parseHealthStatus(healthStatus: String): ContainerHealthStatus = when (healthStatus.lowercase().trim()) {
    "starting" -> ContainerHealthStatus.STARTING
    "healthy" -> ContainerHealthStatus.HEALTHY
    "unhealthy" -> ContainerHealthStatus.UNHEALTHY
    else -> ContainerHealthStatus.UNDEFINED
}