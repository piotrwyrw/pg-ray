package org.piotrwyrw.pgray.db

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import org.piotrwyrw.pgray.docker.PostgresContainer

class DatabaseManager {

    fun createDataSourceForContainer(container: PostgresContainer): HikariDataSource {
        return HikariDataSource(HikariConfig().apply {
            jdbcUrl = "jdbc:postgresql://localhost:${container.port}/${PostgresContainer.POSTGRES_DB}"
            username = PostgresContainer.POSTGRES_USER
            password = PostgresContainer.POSTGRES_PASSWORD
            maximumPoolSize = 2
        })
    }

}