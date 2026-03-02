package org.piotrwyrw.pgray

import java.awt.Color

data class Tile(
    val fromX: Int,
    val fromY: Int,
    val toX: Int,
    val toY: Int,
    val viewportColor: Color
)