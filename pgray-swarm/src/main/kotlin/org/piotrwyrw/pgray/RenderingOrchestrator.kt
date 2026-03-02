package org.piotrwyrw.pgray

import kotlin.math.roundToInt

class RenderingOrchestrator {

    private val _tiles = mutableListOf<Tile>()

    val tiles get() = _tiles as List<Tile>

    fun clearTiles() {
        _tiles.clear()
    }

    fun subdivide(width: Int, height: Int, degree: Int) {
        val count = degree * degree
        val tileWidth = width.toDouble() / degree
        val tileHeight = height.toDouble() / degree

        var x: Int = 0
        var y: Int = 0

        for (i in 0 until count) {
            val fromX = x
            val fromY = y

            x += tileWidth.roundToInt()

            val toX = x
            val toY = fromY + tileHeight

            _tiles.add(Tile(fromX, fromY, toX, toY.roundToInt(), randomColor(i, count)))

            if ((i + 1) % degree == 0) {
                x = 0
                y += tileHeight.roundToInt()
            }
        }
    }


}