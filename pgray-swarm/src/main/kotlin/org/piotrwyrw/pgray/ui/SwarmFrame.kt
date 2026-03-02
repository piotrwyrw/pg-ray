package org.piotrwyrw.pgray.ui

import org.piotrwyrw.pgray.RenderingOrchestrator
import java.awt.*
import javax.swing.*
import javax.swing.event.ChangeEvent

class SwarmFrame() : JFrame("PGRay Swarm") {

    private val leftPanel = JPanel().apply {
        layout = GridBagLayout()
        preferredSize = Dimension(400, 0)
    }

    private val orchestrator = RenderingOrchestrator()

    private val rightPanel = JPanel()

    private val horizontalSplit = JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel)

    private val viewport = Viewport(orchestrator)

    private val widthSpinner = JSpinner(SpinnerNumberModel(300, 1, Integer.MAX_VALUE, 1))
    private val heightSpinner = JSpinner(SpinnerNumberModel(150, 1, Integer.MAX_VALUE, 1))
    private val subdivisionSpinner = JSpinner(SpinnerNumberModel(1, 1, 10, 1))

    init {
        { e: ChangeEvent -> updateProperties() }.apply {
            widthSpinner.addChangeListener(this)
            heightSpinner.addChangeListener(this)
            subdivisionSpinner.addChangeListener(this)
        }

        layout = BorderLayout()
        size = Dimension(1500, 900)
        defaultCloseOperation = EXIT_ON_CLOSE
        isResizable = true
        setLocationRelativeTo(null)
        buildGui()

        updateProperties()

        isVisible = true
    }

    private fun updateProperties() {
        val width = widthSpinner.value as Int
        val height = heightSpinner.value as Int

        orchestrator.clearTiles()
        orchestrator.subdivide(width, height, subdivisionSpinner.value as Int)

        viewport.updateAspect(width, height)
    }

    private fun addPropertyControl(gbc: GridBagConstraints, labelText: String, component: JComponent, row: Int) {
        gbc.gridx = 0
        gbc.gridy = row

        gbc.anchor = GridBagConstraints.WEST
        gbc.weightx = 0.0
        gbc.weighty = 0.0

        leftPanel.add(JLabel(labelText), gbc)

        gbc.fill = GridBagConstraints.HORIZONTAL
        gbc.gridx = 1
        gbc.weightx = 1.0
        leftPanel.add(component, gbc)
    }

    private fun buildPropertiesPanel() {
        val gbc = GridBagConstraints().apply {
            insets = Insets(5, 10, 5, 10)
        }

        leftPanel.layout = GridBagLayout()

        var row: Int = 0

        addPropertyControl(gbc, "Image Width", widthSpinner, row++)
        addPropertyControl(gbc, "Image Height", heightSpinner, row++)
        addPropertyControl(gbc, "Subdivisions", subdivisionSpinner, row++)

        gbc.gridy = row++
        gbc.gridx = 0
        gbc.weightx = 1.0
        gbc.gridwidth = 2
        gbc.fill = GridBagConstraints.HORIZONTAL
        leftPanel.add(JButton("Render"), gbc)

        gbc.gridx = 0
        gbc.gridy = row++
        gbc.weighty = 1.0
        gbc.gridwidth = 2
        gbc.fill = GridBagConstraints.BOTH

        leftPanel.add(JPanel(), gbc)
    }

    private fun buildViewportPanel() {
        rightPanel.add(viewport)
    }

    private fun buildGui() {
        buildPropertiesPanel()
        buildViewportPanel()

        contentPane.add(horizontalSplit, BorderLayout.CENTER)
    }

}