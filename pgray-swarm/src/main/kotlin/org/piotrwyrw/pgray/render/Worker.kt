/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.render

import com.zaxxer.hikari.HikariDataSource
import org.piotrwyrw.pgray.docker.PostgresContainer
import org.piotrwyrw.pgray.docker.status.WorkerStatus

data class Worker(
    val tile: Tile,
    val container: PostgresContainer,
    val dataSource: HikariDataSource? = null,
    var status: WorkerStatus = WorkerStatus.Companion.stopped()
) {
    fun executeQuery(query: String) {
        if (dataSource == null) {
            throw RuntimeException("Worker ${container.containerId} does not have a data source yet.")
        }

        dataSource.connection.use { connection ->
            connection.createStatement().use { stmt ->
                stmt.execute(query)
            }
        }
    }
}