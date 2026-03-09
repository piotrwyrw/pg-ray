/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.container.status

import org.piotrwyrw.pgray.ui.theming.Theme
import java.awt.Color

enum class ContainerHealthStatus {
    UNDEFINED,
    STARTING,
    HEALTHY,
    UNHEALTHY;

    fun color(): Color {
        val healthColors = Theme.containerHealth
        return when (this) {
            UNDEFINED -> healthColors.undefined
            STARTING -> healthColors.starting
            HEALTHY -> healthColors.healthy
            UNHEALTHY -> healthColors.unhealthy
        }
    }
}

fun parseHealthStatus(healthStatus: String): ContainerHealthStatus = when (healthStatus.lowercase().trim()) {
    "starting" -> ContainerHealthStatus.STARTING
    "healthy" -> ContainerHealthStatus.HEALTHY
    "unhealthy" -> ContainerHealthStatus.UNHEALTHY
    else -> ContainerHealthStatus.UNDEFINED
}