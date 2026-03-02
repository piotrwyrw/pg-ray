package org.piotrwyrw.pgray

import java.awt.Color

fun randomColor(index: Int, total: Int): Color {
    val hue = 0.4f + (index.toFloat() / total) * ((1f - 0.4f) * .25f)
    val saturation = 1.0f
    val brightness = 1.0f
    return Color.getHSBColor(hue, saturation, brightness)
}