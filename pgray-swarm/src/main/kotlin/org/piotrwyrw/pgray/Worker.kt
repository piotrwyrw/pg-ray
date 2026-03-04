package org.piotrwyrw.pgray

import com.zaxxer.hikari.HikariDataSource
import org.piotrwyrw.pgray.docker.PostgresContainer
import org.piotrwyrw.pgray.render.WorkerStatus

data class Worker(
    val tileNumber: Int,
    val container: PostgresContainer,
    val dataSource: HikariDataSource? = null,
    var status: WorkerStatus = WorkerStatus.STOPPED
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