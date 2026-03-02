package org.piotrwyrw.pgray.docker

import com.github.dockerjava.api.model.*
import com.github.dockerjava.core.DefaultDockerClientConfig
import com.github.dockerjava.core.DockerClientImpl
import com.github.dockerjava.httpclient5.ApacheDockerHttpClient
import org.slf4j.LoggerFactory
import java.io.File
import java.time.Duration
import java.util.*

class DockerManager {

    private val logger = LoggerFactory.getLogger(javaClass)
    private var nextPort = 5432

    private val clientConfig = DefaultDockerClientConfig.createDefaultConfigBuilder().build()
    private val httpClient = ApacheDockerHttpClient.Builder()
        .dockerHost(clientConfig.dockerHost)
        .sslConfig(clientConfig.sslConfig)
        .maxConnections(100)
        .connectionTimeout(Duration.ofSeconds(10))
        .responseTimeout(Duration.ofSeconds(45))
        .build()
    private val docker = DockerClientImpl.getInstance(clientConfig, httpClient)

    private val containers = mutableListOf<WorkerContainer>()
    private val containerDataRoot = File("pgray_data")

    init {
        containerDataRoot.mkdirs()
    }

    fun createPostgresContainer(): WorkerContainer {
        val name = "pgray-${UUID.randomUUID()}"
        val port = nextPort++

        val container = docker.createContainerCmd("postgres:14-alpine")
            .withName(name)
            .withEnv(
                "POSTGRES_PASSWORD=${WorkerContainer.POSTGRES_PASSWORD}",
                "POSTGRES_USER=${WorkerContainer.POSTGRES_USER}",
                "POSTGRES_DB=${WorkerContainer.POSTGRES_DB}"
            )
            .withHostConfig(
                HostConfig.newHostConfig()
                    .withPortBindings(
                        PortBinding(
                            Ports.Binding.bindPort(port),
                            ExposedPort(WorkerContainer.POSTGRES_PORT)
                        )
                    )
                    .withBinds(
                        Bind(
                            containerDataRoot.path,
                            Volume(WorkerContainer.POSTGRES_DATA_DIR)
                        )
                    )
            )
            .withHealthcheck(
                HealthCheck()
                    .withTest(listOf("CMD-SHELL", "pg_isready -U ${WorkerContainer.POSTGRES_USER}"))
                    .withInterval(2_000_000_000L)
                    .withTimeout(3_000_000_000L)
                    .withRetries(5)
            )
            .exec()

        logger.info("Container created $name (Exposing $port)")

        val pgContainer = WorkerContainer(container.id, name, port)
        return pgContainer
    }

    fun startContainers() {
        containers.forEach { container ->
            docker.startContainerCmd(container.id)
                .exec()

            logger.info("Starting container ${container.name}")
        }
    }

    fun stopContainers() {
        containers.forEach { container ->
            docker.stopContainerCmd(container.id)
            logger.info("Stopped container ${container.name}")
        }
    }

}