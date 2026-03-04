package org.piotrwyrw.pgray.docker

import com.github.dockerjava.api.exception.NotFoundException
import com.github.dockerjava.api.model.*
import com.github.dockerjava.core.DefaultDockerClientConfig
import com.github.dockerjava.core.DockerClientImpl
import com.github.dockerjava.okhttp.OkDockerHttpClient
import org.piotrwyrw.pgray.render.WorkerStatus
import org.slf4j.LoggerFactory
import java.io.File
import java.time.Duration
import java.util.*

class DockerManager {

    private val logger = LoggerFactory.getLogger(javaClass)
    private var nextPort = 50000

    private val clientConfig = DefaultDockerClientConfig.createDefaultConfigBuilder().build()
    private val httpClient = OkDockerHttpClient.Builder()
        .dockerHost(clientConfig.dockerHost)
        .readTimeout(2_000)
        .connectTimeout(2_000)
        .build()


    private val docker = DockerClientImpl.getInstance(clientConfig, httpClient)

    private val containerDataRoot = File("pgray_data")

    init {
        containerDataRoot.mkdirs()
    }

    fun startContainer(containerId: String): DockerManager {
        logger.info("Starting container $containerId")
        docker.startContainerCmd(containerId).exec()
        return this
    }

    fun stopContainer(containerId: String): DockerManager {
        logger.info("Stopping container $containerId")
        docker.stopContainerCmd(containerId).exec()
        return this
    }

    fun removeContainer(containerId: String): DockerManager {
        logger.info("Removing container $containerId")
        try {
            docker.removeContainerCmd(containerId).exec()
        } catch (e: Exception) {
            logger.warn("Failed to remove container $containerId: ${e.javaClass.simpleName}: ${e.message}", containerId)
        }
        return this
    }

    fun isContainerRunning(containerId: String): WorkerStatus {
        return try {
            val response = docker.inspectContainerCmd(containerId).exec()
            if (response.state?.running == true) WorkerStatus.RUNNING else WorkerStatus.STOPPED
        } catch (_: NotFoundException) {
            WorkerStatus.NOT_EXIST
        } catch (e: Exception) {
            WorkerStatus.NOT_EXIST
        }
    }

    fun createPostgresContainer(): PostgresContainer {
        val name = "pgray-${UUID.randomUUID()}"
        val volumeName = "pgray-volume-${UUID.randomUUID()}"
        val port = nextPort++

        val volume = docker
            .createVolumeCmd()
            .withName(volumeName)
            .exec()

        val container = docker
            .createContainerCmd("postgres:14-alpine")
            .withName(name)
            .withEnv(
                "POSTGRES_PASSWORD=${PostgresContainer.POSTGRES_PASSWORD}",
                "POSTGRES_USER=${PostgresContainer.POSTGRES_USER}",
                "POSTGRES_DB=${PostgresContainer.POSTGRES_DB}"
            )
            .withHostConfig(
                HostConfig.newHostConfig()
                    .withPortBindings(
                        PortBinding(
                            Ports.Binding.bindPort(port),
                            ExposedPort(PostgresContainer.POSTGRES_PORT)
                        )
                    )
                    .withBinds(
                        Bind(volumeName, Volume("/var/lib/postgresql/data"))
                    )
            )
            .withHealthcheck(
                HealthCheck()
                    .withTest(listOf("CMD-SHELL", "pg_isready -U ${PostgresContainer.POSTGRES_USER}"))
                    .withInterval(Duration.ofSeconds(2).toNanos())
                    .withTimeout(Duration.ofSeconds(3).toNanos())
                    .withRetries(5)
            )
            .exec()

        logger.info("Container created $name (Exposing $port)")

        val pgContainer = PostgresContainer(container.id, volume.name, name, port)
        return pgContainer
    }

}