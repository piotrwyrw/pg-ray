/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.container

data class PostgresContainer(
    val containerId: String,
    val volumeName: String,
    val name: String,
    val port: Int
) {
    companion object {
        const val POSTGRES_PASSWORD = "postgres"
        const val POSTGRES_USER = "postgres"
        const val POSTGRES_DB = "postgres"
        const val POSTGRES_PORT = 5432;
        const val POSTGRES_DATA_DIR = "/var/lib/postgresql/data"
    }
}