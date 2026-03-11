/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.db

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import org.piotrwyrw.pgray.docker.PostgresContainer

class DatabaseManager {

    private val rendererScriptResourcePath = "/renderer.sql"
    private val rendererScript: String? by lazy {
        javaClass.getResourceAsStream(rendererScriptResourcePath)
            ?.bufferedReader(Charsets.UTF_8)
            ?.readText()
    }

    init {
        if (rendererScript == null) {
            throw IllegalStateException("Could not read renderer script")
        }
    }

    fun createDataSourceForContainer(container: PostgresContainer): HikariDataSource {
        return HikariDataSource(HikariConfig().apply {
            jdbcUrl = "jdbc:postgresql://localhost:${container.port}/${PostgresContainer.POSTGRES_DB}"
            username = PostgresContainer.POSTGRES_USER
            password = PostgresContainer.POSTGRES_PASSWORD
            maximumPoolSize = 2
        })
    }

}