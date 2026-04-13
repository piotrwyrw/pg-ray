plugins {
    kotlin("jvm") version "2.3.0"
    application
    id("org.beryx.runtime") version "2.0.1"
}

group = "org.piotrwyrw"
version = "1.0.0"

repositories {
    mavenCentral()
    maven("https://jitpack.io")
}

dependencies {
    implementation("com.formdev:flatlaf:3.7.1")
    implementation("com.formdev:flatlaf-intellij-themes:3.7.1")

    implementation("com.github.Dansoftowner:jSystemThemeDetector:3.6")

    implementation("org.slf4j:slf4j-api:2.0.9")
    implementation("ch.qos.logback:logback-classic:1.4.11")

    implementation("com.github.docker-java:docker-java-core:3.7.0")
    implementation("com.github.docker-java:docker-java-transport-okhttp:3.7.0")

    implementation("com.zaxxer:HikariCP:7.0.2")
    implementation("org.postgresql:postgresql:42.7.10")

    testImplementation(kotlin("test"))
}

application {
    mainClass.set("org.piotrwyrw.pgray.MainKt")

    applicationDefaultJvmArgs = listOf(
        "-Dsun.java2d.metal=false",
        "-Dsun.java2d.opengl=false"
    )
}

kotlin {
    jvmToolchain(17)
}

runtime {
    options.set(listOf("--strip-debug", "--compress", "2"))
    jpackage {
        imageName = "Swarm"
        installerType = "dmg"
        jvmArgs.addAll(
            listOf(
                "-Dsun.java2d.metal=false",
                "-Dsun.java2d.opengl=false"
            )
        )
        installerOptions.addAll(
            listOf(
                "--app-version", project.version.toString(),
                "--vendor", "Piotr K. Wyrwas",
                "--license-file", "../LICENSE"
            )
        )
    }
}

tasks.test {
    useJUnitPlatform()
}