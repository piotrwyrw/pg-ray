/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.window.swarm

import com.formdev.flatlaf.util.SystemInfo
import org.piotrwyrw.pgray.apply
import org.piotrwyrw.pgray.docker.status.ContainerStatus
import org.piotrwyrw.pgray.docker.status.WorkerStatus
import org.piotrwyrw.pgray.render.Worker
import org.piotrwyrw.pgray.render.contract.IOrchestrator
import org.piotrwyrw.pgray.render.contract.IOrchestratorListener
import org.piotrwyrw.pgray.ui.*
import org.piotrwyrw.pgray.ui.component.GradientPanel
import org.piotrwyrw.pgray.ui.component.Viewport
import org.piotrwyrw.pgray.ui.theming.PropertyBinder.bind
import org.piotrwyrw.pgray.ui.theming.Theme
import org.piotrwyrw.pgray.ui.theming.ThemeMode
import org.piotrwyrw.pgray.ui.theming.useSystemTheme
import org.piotrwyrw.pgray.ui.theming.useTheme
import org.piotrwyrw.pgray.ui.window.BaseWindow
import org.piotrwyrw.pgray.ui.window.dialog.TextViewDialogWindow
import java.awt.Dimension
import java.awt.GraphicsEnvironment
import java.awt.GridBagConstraints
import java.awt.GridBagLayout
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import java.util.concurrent.Executors
import javax.swing.*
import javax.swing.event.ChangeEvent
import javax.swing.table.DefaultTableCellRenderer

class SwarmWindow(
    val orchestrator: IOrchestrator
) : BaseWindow("PGRay Swarm") {

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
        add(JMenu("Appearance") apply {
            add(JMenuItem("Use Dark Theme") apply {
                addActionListener {
                    useTheme(ThemeMode.DARK)
                }
            })
            add(JMenuItem("Use Light Theme") apply {
                addActionListener {
                    useTheme(ThemeMode.LIGHT)
                }
            })
            add(JMenuItem("Use System Theme") apply {
                addActionListener {
                    useSystemTheme()
                }
            })
        })
    }

    private val titleBarColor = Theme.TitleBar.titleBarColor

    private val gui = object {
        val leftPanel = JPanel().apply {
            layout = GridBagLayout()
            preferredSize = Dimension(400, 0)
        }

        val rightPanel = JPanel().apply {
            layout = GridBagLayout()
        }

        val horizontalSplit = JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel)

        val viewport = Viewport(orchestrator).apply {
            border = BorderFactory.createLineBorder(Theme.Accent.accentColor, 2, true)
        }

        val widthSpinner = JSpinner(SpinnerNumberModel(300, 1, Integer.MAX_VALUE, 1))
        val heightSpinner = JSpinner(SpinnerNumberModel(150, 1, Integer.MAX_VALUE, 1))
        val subdivisionSpinner = JSpinner(SpinnerNumberModel(1, 1, 10, 1))

        val workerCountLabel = JLabel("").apply { horizontalAlignment = JLabel.RIGHT }
        val aspectRatioLabel = JLabel("").apply { horizontalAlignment = JLabel.RIGHT }

        val renderButton = JButton("Render")
        val abortButton = JButton("Abort").apply { isEnabled = false }

        val statusBar = GradientPanel(
            GradientPanel.GradientDirection.TOP_DONW,
            Theme.StatusBar.statusBarColor,
            Theme.Surface.ground
        ).apply {
            layout = GridBagLayout()

            bind({ from = it }) { Theme.StatusBar.statusBarColor }
            bind({ to = it }) { Theme.Surface.ground }

            computedOffset { panel ->
                panel.height.toFloat() - panel.height / 6f
            }
        }
        val statusLabel = JLabel()
        val statusBarProgressBar = JProgressBar(JProgressBar.HORIZONTAL).apply {
            value = 0
            preferredSize = Dimension(0, 4)
        }

        val defaultRenderer = DefaultTableCellRenderer().apply {
            horizontalAlignment = DefaultTableCellRenderer.CENTER
        }

        val workerTableModel = TypedTableModel().apply {
            addTypedColumn(WorkerStatus::class.java, "Status")
            addTypedColumn(String::class.java, "Tile")
            addTypedColumn(String::class.java, "Status Description")
        }

        val workerTable = JTable(workerTableModel).apply {
            setDefaultRenderer(String::class.java, defaultRenderer)
            setDefaultRenderer(WorkerStatus::class.java, WorkerStatusTableCellRenderer())
        }

        val workersScrollPane = JScrollPane(workerTable)
    }

    private val cachedWorkers = mutableMapOf<String, Worker>()

    fun create() {
        val bounds = GraphicsEnvironment.getLocalGraphicsEnvironment().maximumWindowBounds

        layout = GridBagLayout()
        size = bounds.size
        location = bounds.location

        buildUI()

        configureListeners()

        applyMacOsFeatures(rootPane)
    }

    private fun buildUI() {
        jMenuBar = menuBar
        buildGui()
        updateUI()
        setStatus()
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

        addWindowListener(object : WindowAdapter() {
            override fun windowClosing(e: WindowEvent?) {
                onWindowClosing()
            }
        })

        orchestrator.subscribe(object : IOrchestratorListener {
            override fun onWorkerCreated(worker: Worker) = orchestrator.startWorker(worker)

            private fun updateWorkerTableModel() {
                gui.workerTableModel.rowCount = 0
                cachedWorkers.values.sortedBy { it.tile.tileNumber }.forEach { worker ->
                    gui.workerTableModel.addRow(
                        arrayOf(
                            worker.status,
                            "Tile ${worker.tile.tileNumber}",
                            worker.status.healthStatus.toString()
                        )
                    )
                }
            }

            override fun onWorkerRemoved(worker: Worker) {
                cachedWorkers.remove(worker.container.containerId)
                updateWorkerTableModel()
                gui.viewport.repaint()
            }

            override fun onWorkerStatusRetrieved(worker: Worker) {
                val id = worker.container.containerId

                if (worker.status.containerStatus == ContainerStatus.ABSENT) {
                    return
                }

                cachedWorkers[id] = worker
                updateWorkerTableModel()
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

        gbc(0, 4).smInsets.fillBoth.let { gbc ->
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
                add(gui.viewport)
            }, gbc)
        }
    }

}