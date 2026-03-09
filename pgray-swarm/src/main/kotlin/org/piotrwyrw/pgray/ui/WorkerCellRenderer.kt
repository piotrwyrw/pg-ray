/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui

import org.piotrwyrw.pgray.render.Worker
import org.piotrwyrw.pgray.container.status.ContainerHealthStatus
import org.piotrwyrw.pgray.container.status.ContainerStatus
import java.awt.Component
import java.awt.Dimension
import java.awt.Graphics
import java.awt.GridBagLayout
import javax.swing.JLabel
import javax.swing.JList
import javax.swing.JPanel
import javax.swing.ListCellRenderer
import kotlin.math.min

class WorkerCellRenderer() : ListCellRenderer<Worker> {
    override fun getListCellRendererComponent(
        list: JList<out Worker>,
        value: Worker,
        index: Int,
        isSelected: Boolean,
        cellHasFocus: Boolean
    ): Component {
        return JPanel().apply {
            layout = GridBagLayout()

            gbc {
                ipadx = 10
                ipady = ipadx
            }.let { gbc ->
                add(object : JPanel() {
                    override fun paintComponent(g: Graphics) {
                        super.paintComponent(g)

                        val containerStatus = value.status.containerStatus

                        g.color = containerStatus.color()

                        val drawFunction = if (containerStatus == ContainerStatus.RUNNING)
                            g::fillOval
                        else
                            g::drawOval

                        val size = min(width, height) - 5

                        drawFunction(width / 2 - size / 2, height / 2 - size / 2, size, size)
                    }
                }.apply {
                    preferredSize = Dimension(10, 10)
                    isOpaque = false
                }, gbc)
            }

            val healthStatus = value.status.healthStatus

            gbc(1) {
                ipadx = 10
                ipady = ipadx
            }.let { gbc ->
                add(object : JPanel() {
                    override fun paintComponent(g: Graphics) {
                        super.paintComponent(g)

                        g.color = healthStatus.color()

                        val drawFunction = if (healthStatus == ContainerHealthStatus.HEALTHY)
                            g::fillOval
                        else
                            g::drawOval

                        val size = min(width, height) - 5

                        drawFunction(width / 2 - size / 2, height / 2 - size / 2, size, size)
                    }
                }.apply {
                    preferredSize = Dimension(10, 10)
                    isOpaque = false
                }, gbc)
            }

            gbc(2).smInsets.fillBoth.let { gbc ->
                add(JLabel("Tile ${value.tile.tileNumber}"), gbc)
            }

            placeholderPanel(3)

            gbc(4).smInsets.fillHorizontal.let { gbc ->
                add(JLabel(healthStatus.toString().uppercase()).apply {
                    foreground = healthStatus.color()
                }, gbc)
            }

            placeholderPanel(5)
        }
    }
}