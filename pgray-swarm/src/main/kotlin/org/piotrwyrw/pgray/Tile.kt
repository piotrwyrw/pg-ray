package org.piotrwyrw.pgray

import java.awt.Color

val tileColorFrom = Color.decode("#2ecc71")
val tileColorTo = Color.decode("#70a1ff")

data class Tile(
    val fromX: Int,
    val fromY: Int,
    val toX: Int,
    val toY: Int,
    val viewportColor: Color
)

fun tileColor(index: Int, total: Int): Color {
    val rgbFrom = floatArrayOf(tileColorFrom.red.toFloat(), tileColorFrom.green.toFloat(), tileColorFrom.blue.toFloat())
    val rgbTo = floatArrayOf(tileColorTo.red.toFloat(), tileColorTo.green.toFloat(), tileColorTo.blue.toFloat())

    val t = index.toFloat() / total.toFloat()

    val rgb = floatArrayOf(
        rgbFrom[0] + (rgbTo[0] - rgbFrom[0]) * t,
        rgbFrom[1] + (rgbTo[1] - rgbFrom[1]) * t,
        rgbFrom[2] + (rgbTo[2] - rgbFrom[2]) * t,
    )

    return Color(rgb[0].toInt(), rgb[1].toInt(), rgb[2].toInt())
}
