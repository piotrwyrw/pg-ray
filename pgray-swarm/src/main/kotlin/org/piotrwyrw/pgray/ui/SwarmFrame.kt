/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui

import org.piotrwyrw.pgray.Worker
import org.piotrwyrw.pgray.container.status.ContainerStatus
import org.piotrwyrw.pgray.render.RenderingOrchestrator
import org.piotrwyrw.pgray.render.listener.RenderingOrchestratorListener
import org.piotrwyrw.pgray.ui.listener.SimplifiedWindowListener
import org.piotrwyrw.pgray.ui.theming.ThemeColors
import org.slf4j.LoggerFactory
import java.awt.BasicStroke
import java.awt.Color
import java.awt.Dimension
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import java.awt.Paint
import java.awt.event.WindowEvent
import java.util.concurrent.Executors
import javax.swing.*
import javax.swing.border.BevelBorder
import javax.swing.border.TitledBorder
import javax.swing.event.ChangeEvent

class SwarmFrame(
    val orchestrator: RenderingOrchestrator
) : JFrame("PGRay Swarm") {

    private val initialWidth = 1500
    private val initialHeight = 900

    private val logger = LoggerFactory.getLogger(javaClass)

    private val gui = object {
        val leftPanel = JPanel().apply {
            layout = GridBagLayout()
            preferredSize = Dimension(400, 0)
        }

        val rightPanel = JPanel().apply {
            layout = GridBagLayout()
            background = ThemeColors.surface.LAYER1
        }

        val horizontalSplit = JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel)

        val viewport = Viewport(orchestrator).apply {
            border = BorderFactory.createLineBorder(ThemeColors.accent, 2)
        }

        val widthSpinner = JSpinner(SpinnerNumberModel(300, 1, Integer.MAX_VALUE, 1))
        val heightSpinner = JSpinner(SpinnerNumberModel(150, 1, Integer.MAX_VALUE, 1))
        val subdivisionSpinner = JSpinner(SpinnerNumberModel(1, 1, 10, 1))

        val workerCountLabel = JLabel("").apply { horizontalAlignment = JLabel.RIGHT }
        val aspectRatioLabel = JLabel("").apply { horizontalAlignment = JLabel.RIGHT }

        val renderButton = JButton("Render")
        val abortButton = JButton("Abort").apply { isEnabled = false }

        val statusBar = JPanel().apply {
            layout = GridBagLayout()
            background = ThemeColors.surface.LAYER2
        }
        val statusLabel = JLabel()
        val statusBarProgressBar = JProgressBar(JProgressBar.HORIZONTAL).apply {
            value = 0
            preferredSize = Dimension(0, 4)
        }

        val workerListModel = DefaultListModel<Worker>()
        val workerList = JList(workerListModel).apply {
            cellRenderer = WorkerCellRenderer()
            background = ThemeColors.surface.GROUND
        }

        val workersScrollPane = JScrollPane(workerList)
    }

    private val cachedWorkers = mutableMapOf<String, Worker>()

    init {
        layout = GridBagLayout()
        size = Dimension(initialWidth, initialHeight)
        defaultCloseOperation = DO_NOTHING_ON_CLOSE
        isResizable = true
        setLocationRelativeTo(null)

        buildGui()
        updateUI()
        configureListeners()
        setStatus()

        isVisible = true
    }

    private fun startRender() {
        gui.subdivisionSpinner.isEnabled = false
        gui.renderButton.isEnabled = false
        gui.widthSpinner.isEnabled = false
        gui.heightSpinner.isEnabled = false
        updateUI()
        orchestrator.createAllWorkers()
        gui.abortButton.isEnabled = true
    }

    private fun abortRender() {
        gui.abortButton.isEnabled = false
        setStatus("Aborting ...")
        killAllWorkers {
            setStatus()
            gui.renderButton.isEnabled = true
            gui.subdivisionSpinner.isEnabled = true
            gui.widthSpinner.isEnabled = true
            gui.heightSpinner.isEnabled = true
        }
    }

    private fun killAllWorkers(then: () -> Unit) {
        Executors.newSingleThreadExecutor().submit {
            orchestrator.stopAllWorkers()
            orchestrator.removeAllWorkers()
            orchestrator.awaitCompletion()
            SwingUtilities.invokeLater {
                then()
            }
        }
    }

    private fun configureListeners() {
        { _: ChangeEvent ->
            updateUI()
        }.apply {
            gui.widthSpinner.addChangeListener(this@apply)
            gui.heightSpinner.addChangeListener(this@apply)
            gui.subdivisionSpinner.addChangeListener(this@apply)
        }

        addWindowListener(object : SimplifiedWindowListener {
            override fun windowClosing(e: WindowEvent) {
                setStatus("Killing all workers ...", 0)
                isEnabled = false
                killAllWorkers {
                    System.exit(0)
                }
            }
        })

        orchestrator.subscribe(object : RenderingOrchestratorListener {
            override fun onWorkerCreated(worker: Worker) = orchestrator.startWorker(worker)

            private fun updateWorkerListModel() {
                val selectedIndex = gui.workerList.selectedIndex
                gui.workerListModel.clear()
                gui.workerListModel.addAll(cachedWorkers.values.sortedBy { it.tile.tileNumber })
                if (selectedIndex < gui.workerListModel.size)
                    gui.workerList.selectedIndex = selectedIndex
            }

            override fun onWorkerRemoved(worker: Worker) {
                cachedWorkers.remove(worker.container.containerId)
                updateWorkerListModel()
                gui.viewport.repaint()
            }

            override fun onWorkerStatusRetrieved(worker: Worker) {
                val id = worker.container.containerId

                if (worker.status.containerStatus == ContainerStatus.ABSENT) {
                    return
                }

                cachedWorkers[id] = worker
                updateWorkerListModel()
                gui.viewport.repaint()
            }
        })


    }

    private fun updateUI() {
        val width = gui.widthSpinner.value as Int
        val height = gui.heightSpinner.value as Int
        val subdivisions = gui.subdivisionSpinner.value as Int

        orchestrator.createRenderingTiles(width, height, subdivisions)

        gui.workerCountLabel.text = (subdivisions * subdivisions).toString()
        gui.aspectRatioLabel.text = (width.toDouble() / height.toDouble()).toString()

        gui.viewport.updateAspect(width, height)
        gui.viewport.repaint()

        gui.viewport.revalidate()

        revalidate()
        repaint()
    }

    private fun setStatus(status: String = "Ready.", progress: Int = 0) {
        gui.statusLabel.text = status

        if (progress <= 0)
            gui.statusBarProgressBar.isVisible = false
        else
            gui.statusBarProgressBar.isVisible = true

        gui.statusBarProgressBar.value = progress
        updateUI()
    }

    private fun buildGui() {
        buildPropertiesPanel()
        buildViewportPanel()
        gbc().fillBoth.let { gbc ->
            add(gui.horizontalSplit, gbc)
        }
    }

    private fun addPropertyControl(
        labelText: String,
        component: JComponent,
        row: Int,
        destination: JPanel
    ) {
        gbc(0, row) {
            anchor = GridBagConstraints.WEST
        }.smInsets.let { gbc ->
            destination.add(JLabel(labelText), gbc)
        }

        gbc(1, row) {
            fill = GridBagConstraints.HORIZONTAL
            weightx = 1.0
        }.smInsets.let { gbc ->
            destination.add(component, gbc)
        }
    }

    private fun buildPropertiesPanel() {
        gui.leftPanel.layout = GridBagLayout()

        val propertiesWrapper = JPanel().apply {
            border = BorderFactory.createTitledBorder("Properties")
            layout = GridBagLayout()

            addPropertyControl("Image Width", gui.widthSpinner, 0, this@apply)
            addPropertyControl("Image Height", gui.heightSpinner, 1, this@apply)
            addPropertyControl("Subdivisions", gui.subdivisionSpinner, 2, this@apply)
        }

        val statsWrapper = JPanel().apply {
            layout = GridBagLayout()
            border = BorderFactory.createTitledBorder("Stats")

            addPropertyControl(
                "Worker Count:",
                gui.workerCountLabel,
                0,
                this@apply
            )

            addPropertyControl(
                "Aspect Ratio:",
                gui.aspectRatioLabel,
                1,
                this@apply
            )
        }

        val actionsWrapper = JPanel().apply {
            layout = GridBagLayout()
            border = BorderFactory.createTitledBorder("Actions")

            gbc().smInsets.fillHorizontal.let { gbc ->
                add(gui.renderButton.apply {
                    addActionListener {
                        startRender()
                    }
                }, gbc)
            }

            gbc(0, 1).smInsets.fillHorizontal.let { gbc ->
                add(gui.abortButton.apply {
                    addActionListener {
                        abortRender()
                    }
                }, gbc)
            }

        }

        val workerListWrapper = JPanel().apply {
            layout = GridBagLayout()
            border = BorderFactory.createTitledBorder("Workers")

            gbc().smInsets.fillBoth.let { gbc ->
                add(gui.workersScrollPane, gbc)
            }
        }

        gbc().smInsets.fillHorizontal.let { gbc ->
            gui.leftPanel.add(propertiesWrapper, gbc)
        }

        gbc(0, 1).smInsets.fillHorizontal.let { gbc ->
            gui.leftPanel.add(statsWrapper, gbc)
        }

        gbc(0, 2).smInsets.fillHorizontal.let { gbc ->
            gui.leftPanel.add(statsWrapper, gbc)
        }

        gbc(0, 3).smInsets.fillHorizontal.let { gbc ->
            gui.leftPanel.add(actionsWrapper, gbc)
        }

        gbc(0, 4).fillBoth.let { gbc ->
            gui.leftPanel.add(workerListWrapper, gbc)
        }
    }

    private fun buildStatusBar(): JPanel {
        gbc {
            anchor = GridBagConstraints.WEST
        }.smInsets.fillBoth.let { gbc ->
            gui.statusBar.add(gui.statusLabel, gbc)
        }

        gbc(1) {
            anchor = GridBagConstraints.WEST
        }.smInsets.fillBoth.let { gbc ->
            gui.statusBar.add(gui.statusBarProgressBar, gbc)
        }

        return gui.statusBar
    }

    private fun buildViewportPanel() {
        gbc().fillHorizontal.let { gbc ->
            gui.rightPanel.add(buildStatusBar(), gbc)
        }

        gbc(0, 1).xlInsets.fillBoth.let { gbc ->
            gui.rightPanel.add(JPanel().apply {
                background = ThemeColors.surface.LAYER1
                add(gui.viewport)
            }, gbc)
        }
    }

}