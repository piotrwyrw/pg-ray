/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray

import java.awt.Color
import java.time.Duration
import java.util.concurrent.TimeUnit

fun Duration.toPrettyString(): String {
    val days = this.toDays()
    val hours = this.toHours() % 24
    val minutes = this.toMinutes() % 60
    val seconds = this.seconds % 60
    val millis = this.toMillis() % 1000

    return buildString {
        if (days > 0) append("${days}d ")
        if (hours > 0) append("${hours}h ")
        if (minutes > 0) append("${minutes}m ")
        if (seconds > 0 || isZero) append("${seconds}s ")
        if (millis > 0) append("${millis}ms")
    }.trim()
}

operator fun Duration.component1(): Long {
    return this.toMillis()
}

operator fun Duration.component2(): TimeUnit {
    return TimeUnit.MILLISECONDS
}

operator fun String.invoke(): Color {
    if (startsWith('#')) {
        return Color.decode(this)
    }

    return Color.decode("#$this")
}

operator fun Color.component1() = red
operator fun Color.component2() = green
operator fun Color.component3() = blue

fun Color.brightness(brightness: Double): Color {
    val (r, g, b) = this
    val rgbRange = 0.0 .. 255.0
    return Color(
        (red * brightness).coerceIn(rgbRange).toInt(),
        (green * brightness).coerceIn(rgbRange).toInt(),
        (blue * brightness).coerceIn(rgbRange).toInt()
    )
}

infix fun<T> T.apply(block: T.() -> Unit): T {
    this.block()
    return this
}