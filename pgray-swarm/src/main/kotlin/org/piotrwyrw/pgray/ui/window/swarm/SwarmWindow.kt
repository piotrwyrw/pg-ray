/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.window.swarm

import com.formdev.flatlaf.util.SystemInfo
import org.piotrwyrw.pgray.apply
import org.piotrwyrw.pgray.docker.status.ContainerStatus
import org.piotrwyrw.pgray.render.Worker
import org.piotrwyrw.pgray.render.contract.IOrchestrator
import org.piotrwyrw.pgray.render.contract.IOrchestratorListener
import org.piotrwyrw.pgray.ui.*
import org.piotrwyrw.pgray.ui.component.Viewport
import org.piotrwyrw.pgray.ui.theming.Theme
import org.piotrwyrw.pgray.ui.window.dialog.TextViewDialogWindow
import java.awt.Dimension
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.util.concurrent.Executors
import javax.swing.*
import javax.swing.event.ChangeEvent

class SwarmWindow(
    val orchestrator: IOrchestrator
) : JFrame("PGRay Swarm") {

    companion object {
        const val INITIAL_WIDTH = 1500
        const val INITIAL_HEIGHT = 900
    }

    private val licenseText by lazy {
        val stream = javaClass.getResourceAsStream("/license.txt") ?: return@lazy "Could not load license"
        return@lazy stream.bufferedReader().readText()
    }

    private val menuBar = JMenuBar() apply {
        add(JMenu("Help") apply {
            add(JMenuItem("License") apply {
                addActionListener {
                    TextViewDialogWindow(this@SwarmWindow, "GNU General Public License v3", licenseText)
                        .create()
                }
            })
        })
    }

    private val titleBarColor = Theme.accent.titleBarColor

    private val gui = object {
        val leftPanel = JPanel().apply {
            layout = GridBagLayout()
            preferredSize = Dimension(400, 0)
        }

        val rightPanel = JPanel().apply {
            layout = GridBagLayout()
            background = Theme.surface.layer1
        }

        val horizontalSplit = JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel)

        val viewport = Viewport(orchestrator).apply {
            border = BorderFactory.createLineBorder(Theme.accent.accentColor, 2, true)
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
            background = Theme.surface.layer2
        }
        val statusLabel = JLabel()
        val statusBarProgressBar = JProgressBar(JProgressBar.HORIZONTAL).apply {
            value = 0
            preferredSize = Dimension(0, 4)
        }

        val workerListModel = DefaultListModel<Worker>()
        val workerList = JList(workerListModel).apply {
            cellRenderer = WorkerCellRenderer()
            background = Theme.surface.ground
        }

        val workersScrollPane = JScrollPane(workerList)
    }

    private val cachedWorkers = mutableMapOf<String, Worker>()

    fun create() {
        layout = GridBagLayout()
        size = Dimension(INITIAL_WIDTH, INITIAL_HEIGHT)

        buildUI()

        addWindowListener(object : WindowAdapter() {
            override fun windowClosing(e: WindowEvent?) {
                onWindowClosing()
            }
        })

        if (SystemInfo.isMacFullWindowContentSupported) {
            rootPane.putClientProperty("apple.awt.fullWindowContent", true);
            rootPane.putClientProperty("apple.awt.transparentTitleBar", true)
        }

        setLocationRelativeTo(null)
    }

    private fun buildUI() {
        jMenuBar = menuBar
        buildGui()
        updateUI()
        configureListeners()
        setStatus()
        orchestrator.startInspectionThread()
    }

    private fun onWindowClosing() {
        setStatus("Killing all workers ...", 0)
        isEnabled = false
        killAllWorkers {
            System.exit(0)
        }
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

        orchestrator.subscribe(object : IOrchestratorListener {
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
        gui.statusBarProgressBar.isVisible = progress > 0
        gui.statusBarProgressBar.value = progress
        updateUI()
    }

    private fun buildGui() {
        buildPropertiesPanel()
        buildViewportPanel()

        gbc().fillBoth.let { gbc ->
            if (SystemInfo.isMacOS) {
                add(JPanel().apply {
                    background = titleBarColor
                    layout = GridBagLayout()
                    gbc { insets = Insets(28, 0, 0, 0) }.fillBoth.let { gbc ->
                        add(gui.horizontalSplit, gbc)
                    }
                }, gbc)
            } else {
                add(gui.horizontalSplit, gbc)
            }
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
                background = Theme.surface.layer1
                add(gui.viewport)
            }, gbc)
        }
    }

}