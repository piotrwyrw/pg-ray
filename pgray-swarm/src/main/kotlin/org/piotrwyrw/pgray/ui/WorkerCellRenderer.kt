package org.piotrwyrw.pgray.ui

import org.piotrwyrw.pgray.Worker
import org.piotrwyrw.pgray.render.WorkerStatus
import java.awt.*
import javax.swing.JLabel
import javax.swing.JList
import javax.swing.JPanel
import javax.swing.ListCellRenderer
import kotlin.math.min

class WorkerCellRenderer : ListCellRenderer<Worker> {
    override fun getListCellRendererComponent(
        list: JList<out Worker>,
        value: Worker,
        index: Int,
        isSelected: Boolean,
        cellHasFocus: Boolean
    ): Component {
        return JPanel().apply {
            val gbc = GridBagConstraints()
            layout = GridBagLayout()

            gbc.weightx = 0.0
            gbc.weighty = 0.0
            gbc.gridx = 0
            gbc.gridy = 0
            gbc.ipadx = 10
            gbc.ipady = gbc.ipadx
            add(object : JPanel() {
                override fun paintComponent(g: Graphics) {
                    super.paintComponent(g)

                    g.color = when (value.status) {
                        WorkerStatus.RUNNING -> Color.green
                        WorkerStatus.STOPPED -> Color.red
                        WorkerStatus.NOT_EXIST -> Color.blue
                    }

                    val drawFunction = if (value.status == WorkerStatus.RUNNING)
                        g::fillOval
                    else
                        g::drawOval

                    val size = min(width, height) - 5

                    drawFunction(width / 2 - size / 2, height / 2 - size / 2, size, size)
                }
            }.apply {
                preferredSize = Dimension(10, 10)
                toolTipText = value.status.toString()
            }, gbc)

            gbc.gridx = 1
            gbc.weightx = 1.0
            gbc.weighty = 1.0
            gbc.ipadx = 0
            gbc.ipady = 0
            gbc.fill = GridBagConstraints.BOTH
            gbc.insets = Insets(0, 20, 0, 0)
            add(JLabel("Tile ${value.tileNumber} (${value.status})"), gbc)
        }
    }
}
