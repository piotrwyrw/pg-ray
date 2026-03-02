plugins {
    kotlin("jvm") version "2.3.0"
}

group = "org.piotrwyrw"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.slf4j:slf4j-api:2.0.9")
    implementation("ch.qos.logback:logback-classic:1.4.11")

    implementation("com.github.docker-java:docker-java-core:3.7.0")
    implementation("com.github.docker-java:docker-java-transport-httpclient5:3.7.0")

    implementation("com.github.weisj:darklaf-core:3.1.1")

    implementation("com.zaxxer:HikariCP:7.0.2")
    implementation("org.postgresql:postgresql:42.7.10")

    testImplementation(kotlin("test"))
}

kotlin {
    jvmToolchain(17)
}

tasks.test {
    useJUnitPlatform()
}