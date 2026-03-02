package org.piotrwyrw.pgray.db

import com.zaxxer.hikari.HikariConfig
import com.zaxxer.hikari.HikariDataSource
import org.piotrwyrw.pgray.docker.WorkerContainer
import javax.sql.DataSource

class DatabaseManager() {

    private val dataSources = hashMapOf<String, DataSource>()

    fun createDataSourceForContainer(container: WorkerContainer) {
        dataSources[container.id] = HikariDataSource(HikariConfig().apply {
            jdbcUrl = "jdbc:postgresql://localhost:${container.port}/${WorkerContainer.POSTGRES_DB}"
            username = WorkerContainer.POSTGRES_USER
            password = WorkerContainer.POSTGRES_PASSWORD
            maximumPoolSize = 2
        })
    }

    fun executeQuery(containerId: String, query: String) {
        val ds = dataSources[containerId] ?: throw RuntimeException("No data source exists for container $containerId")
        ds.connection.use { connection ->
            connection.createStatement().use { stmt ->
                stmt.execute(query)
            }
        }
    }

}