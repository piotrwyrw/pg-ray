/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray

import java.awt.Color

data class Tile(
    val tileNumber: Int,
    val fromX: Int,
    val fromY: Int,
    val toX: Int,
    val toY: Int,
) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Tile

        if (tileNumber != other.tileNumber) return false
        if (fromX != other.fromX) return false
        if (fromY != other.fromY) return false
        if (toX != other.toX) return false
        if (toY != other.toY) return false

        return true
    }

    override fun hashCode(): Int {
        var result = tileNumber
        result = 31 * result + fromX
        result = 31 * result + fromY
        result = 31 * result + toX
        result = 31 * result + toY
        return result
    }
}