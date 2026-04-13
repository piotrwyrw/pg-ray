/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui

import javax.swing.table.DefaultTableModel

class TypedTableModel : DefaultTableModel() {
    private val columnTypes = mutableMapOf<Int, Class<*>>()

    fun addTypedColumn(type: Class<*>, name: String) {
        val index = columnCount
        columnTypes[index] = type
        addColumn(name)
    }

    override fun getColumnClass(columnIndex: Int): Class<*> = columnTypes[columnIndex] ?: String::class.java
}