/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.docker.status

data class WorkerStatus(
    var containerStatus: ContainerStatus,
    var healthStatus: ContainerHealthStatus = ContainerHealthStatus.HEALTHY,
) {
    companion object {
        fun stopped() = WorkerStatus(ContainerStatus.STOPPED, ContainerHealthStatus.UNDEFINED)
    }
}