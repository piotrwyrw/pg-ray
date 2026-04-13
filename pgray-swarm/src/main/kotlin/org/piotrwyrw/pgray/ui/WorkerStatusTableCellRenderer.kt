/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui

import org.piotrwyrw.pgray.docker.status.WorkerStatus
import org.piotrwyrw.pgray.ui.theming.Theme
import java.awt.Component
import java.awt.Graphics
import javax.swing.JPanel
import javax.swing.JTable
import javax.swing.table.DefaultTableCellRenderer
import kotlin.math.min

class WorkerStatusTableCellRenderer : DefaultTableCellRenderer() {
    init {
        horizontalAlignment = CENTER
    }

    override fun getTableCellRendererComponent(
        table: JTable?,
        value: Any?,
        isSelected: Boolean,
        hasFocus: Boolean,
        row: Int,
        column: Int
    ): Component? {
        val status = value as? WorkerStatus ?: return null
        return object : JPanel() {
            override fun paintComponent(g: Graphics) {
                if (isSelected) {
                    g.color = Theme.List.selectedBackground
                    g.fillRect(0, 0, width, height)
                }

                val size = min(width, height) / 2
                g.color = status.healthStatus.color()
                g.fillOval(width / 2 - size / 2, height / 2 - size / 2, size, size)
            }
        }.apply {
            isOpaque = false
        }
    }
}