package org.piotrwyrw.pgray.ui

import org.piotrwyrw.pgray.Tile
import org.piotrwyrw.pgray.Worker
import org.piotrwyrw.pgray.render.RenderingOrchestrator
import org.piotrwyrw.pgray.render.WorkerStatus
import org.piotrwyrw.pgray.render.listener.RenderingOrchestratorListener
import org.slf4j.LoggerFactory
import java.awt.*
import java.awt.event.WindowEvent
import java.awt.event.WindowListener
import java.util.concurrent.Executors
import javax.swing.*
import javax.swing.event.ChangeEvent
import kotlin.system.exitProcess

class SwarmFrame(
    val orchestrator: RenderingOrchestrator
) : JFrame("PGRay Swarm") {

    private val logger = LoggerFactory.getLogger(javaClass)

    private val statusBarBackground = (UIManager.getColor("background") ?: Color.white).darker()

    private val leftPanel = JPanel().apply {
        layout = GridBagLayout()
        preferredSize = Dimension(400, 0)
    }

    private val rightPanel = JPanel().apply {
        layout = GridBagLayout()
    }

    private val horizontalSplit = JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel)

    private val viewport = Viewport(orchestrator)

    private val widthSpinner = JSpinner(SpinnerNumberModel(300, 1, Integer.MAX_VALUE, 1))
    private val heightSpinner = JSpinner(SpinnerNumberModel(150, 1, Integer.MAX_VALUE, 1))
    private val subdivisionSpinner = JSpinner(SpinnerNumberModel(1, 1, 10, 1))

    private val workerCountLabel = JLabel("").apply { horizontalAlignment = JLabel.RIGHT }
    private val aspectRatioLabel = JLabel("").apply { horizontalAlignment = JLabel.RIGHT }

    private val statusLabel = JLabel("Ready.")
    private val statusBarProgressBar = JProgressBar(JProgressBar.HORIZONTAL).apply {
        value = 0
        preferredSize = Dimension(0, 4)
    }

    val workerListModel = DefaultListModel<Worker>()
    private val workerList = JList(workerListModel).apply {
        cellRenderer = WorkerCellRenderer()
    }
    private val cachedWorkers = mutableMapOf<String, Worker>()

    init {
        { _: ChangeEvent -> updateUI() }.apply {
            widthSpinner.addChangeListener(this@apply)
            heightSpinner.addChangeListener(this@apply)
            subdivisionSpinner.addChangeListener(this@apply)
        }

        addWindowListener(object : WindowListener {
            override fun windowOpened(e: WindowEvent) = Unit
            override fun windowClosing(e: WindowEvent) {
                setStatus("Killing all workers ...", 0)
                isEnabled = false
                Executors.newSingleThreadExecutor().submit {
                    orchestrator.stopAllWorkers()
                    Thread.sleep(5000)
                    orchestrator.removeAllWorkers()
                    orchestrator.awaitCompletion(5000)
                    SwingUtilities.invokeLater {
                        exitProcess(0)
                    }
                }
            }

            override fun windowClosed(e: WindowEvent?) = Unit
            override fun windowIconified(e: WindowEvent?) = Unit
            override fun windowDeiconified(e: WindowEvent?) = Unit
            override fun windowActivated(e: WindowEvent?) = Unit
            override fun windowDeactivated(e: WindowEvent?) = Unit
        })

        orchestrator.subscribe(object : RenderingOrchestratorListener {
            override fun onRenderingTilesCreated(imageWidth: Int, imageHeight: Int, tiles: List<Tile>) = Unit
            override fun onRenderingTilesCleared() = Unit
            override fun onWorkerCreated(worker: Worker) = orchestrator.startWorker(worker)
            override fun onWorkerRemoved(worker: Worker) = Unit
            override fun onWorkerStarted(worker: Worker) = Unit
            override fun onWorkerStopped(worker: Worker) = Unit

            override fun onWorkerStatusRetrieved(worker: Worker) {
                val id = worker.container.containerId

                if (worker.status == WorkerStatus.NOT_EXIST) {
                    cachedWorkers.remove(id)
                } else cachedWorkers[id] = worker.copy()

                workerListModel.clear()
                workerListModel.addAll(cachedWorkers.values)

                updateUI()
            }
        })

        layout = BorderLayout()
        size = Dimension(1500, 900)
        defaultCloseOperation = DO_NOTHING_ON_CLOSE
        isResizable = true
        setLocationRelativeTo(null)
        buildGui()

        updateUI()

        isVisible = true
    }

    private fun startRender() {
        orchestrator.createAllWorkers()
    }

    private fun updateUI() {
        val width = widthSpinner.value as Int
        val height = heightSpinner.value as Int

        val subdivisions = subdivisionSpinner.value as Int

        orchestrator.createRenderingTiles(width, height, subdivisions)

        viewport.updateAspect(width, height)

        workerCountLabel.text = (subdivisions * subdivisions).toString()

        aspectRatioLabel.text = (width.toDouble() / height.toDouble()).toString()

        workerList.revalidate()
        workerList.repaint()

        statusLabel.revalidate()
        statusLabel.repaint()

        revalidate()
        repaint()
    }

    private fun setStatus(status: String, progress: Int = 0) {
        this.statusLabel.text = status
        this.statusBarProgressBar.value = progress
        updateUI()
    }

    private fun buildGui() {
        buildPropertiesPanel()
        buildViewportPanel()

        contentPane.add(horizontalSplit, BorderLayout.CENTER)
    }

    private fun addPropertyControl(
        gbc: GridBagConstraints,
        labelText: String,
        component: JComponent,
        row: Int,
        destination: JPanel
    ) {
        gbc.gridx = 0
        gbc.gridy = row

        gbc.anchor = GridBagConstraints.WEST
        gbc.weightx = 0.0
        gbc.weighty = 0.0

        destination.add(JLabel(labelText), gbc)

        gbc.fill = GridBagConstraints.HORIZONTAL
        gbc.gridx = 1
        gbc.weightx = 1.0
        destination.add(component, gbc)
    }

    private fun buildPropertiesPanel() {
        leftPanel.layout = GridBagLayout()

        val gbc = GridBagConstraints().apply {
            insets = Insets(5, 10, 5, 10)
        }

        val propertiesWrapper = JPanel().apply {
            val gbc = GridBagConstraints().apply {
                insets = Insets(5, 5, 5, 5)
            }

            border = BorderFactory.createTitledBorder("Properties")
            layout = GridBagLayout()

            addPropertyControl(gbc, "Image Width", widthSpinner, 0, this@apply)
            addPropertyControl(gbc, "Image Height", heightSpinner, 1, this@apply)
            addPropertyControl(gbc, "Subdivisions", subdivisionSpinner, 2, this@apply)
        }

        val statsWrapper = JPanel().apply {
            val gbc = GridBagConstraints().apply {
                insets = Insets(5, 5, 5, 5)
            }

            layout = GridBagLayout()
            border = BorderFactory.createTitledBorder("Stats")

            addPropertyControl(
                gbc,
                "Worker Count:",
                workerCountLabel,
                0,
                this@apply
            )

            addPropertyControl(
                gbc,
                "Aspect Ratio:",
                aspectRatioLabel,
                1,
                this@apply
            )
        }

        val actionsWrapper = JPanel().apply {
            val gbc = GridBagConstraints().apply {
                insets = Insets(5, 5, 5, 5)
            }

            gbc.weightx = 1.0
            gbc.weighty = 0.0
            gbc.fill = GridBagConstraints.HORIZONTAL

            layout = GridBagLayout()
            border = BorderFactory.createTitledBorder("Actions")

            add(JButton("Render").apply {
                addActionListener {
                    startRender()
                }
            }, gbc)
        }

        val workerListWrapper = JPanel().apply {
            val gbc = GridBagConstraints().apply {
                insets = Insets(5, 5, 5, 5)
            }

            gbc.weightx = 1.0
            gbc.weighty = 0.0
            gbc.fill = GridBagConstraints.HORIZONTAL

            layout = GridBagLayout()
            border = BorderFactory.createTitledBorder("Workers")

            add(workerList, gbc)
        }

        gbc.gridx = 0
        gbc.weightx = 1.0
        gbc.weighty = 0.0
        gbc.fill = GridBagConstraints.HORIZONTAL
        gbc.gridy = 0
        leftPanel.add(propertiesWrapper, gbc)

        gbc.gridy = 1
        leftPanel.add(statsWrapper, gbc)

        gbc.gridy = 2
        gbc.gridx = 0
        gbc.weightx = 1.0
        gbc.gridwidth = 2
        gbc.fill = GridBagConstraints.HORIZONTAL
        leftPanel.add(statsWrapper, gbc)

        gbc.gridy = 3
        gbc.gridx = 0
        gbc.weightx = 1.0
        gbc.gridwidth = 2
        gbc.fill = GridBagConstraints.HORIZONTAL
        leftPanel.add(actionsWrapper, gbc)

        gbc.gridy = 4
        gbc.gridx = 0
        gbc.weightx = 1.0
        gbc.gridwidth = 2
        gbc.fill = GridBagConstraints.HORIZONTAL
        leftPanel.add(workerListWrapper, gbc)

        gbc.gridy = 5
        gbc.gridx = 0
        gbc.weighty = 1.0
        gbc.gridwidth = 2
        gbc.fill = GridBagConstraints.BOTH

        leftPanel.add(JPanel(), gbc)
    }

    private fun buildStatusBar(): JPanel {
        val panel = JPanel()

        panel.layout = GridBagLayout()

        val gbc = GridBagConstraints()
        gbc.gridx = 0
        gbc.gridy = 0
        gbc.weightx = 1.0
        gbc.weighty = 1.0
        gbc.anchor = GridBagConstraints.WEST
        gbc.fill = GridBagConstraints.BOTH
        gbc.insets = Insets(5, 10, 5, 10)

        panel.add(statusLabel, gbc)

        gbc.gridx = 1
        panel.add(statusBarProgressBar, gbc)

        panel.background = statusBarBackground

        return panel
    }

    private fun buildViewportPanel() {
        val gbc = GridBagConstraints()

        gbc.gridy = 0
        gbc.weightx = 1.0
        gbc.fill = GridBagConstraints.HORIZONTAL
        gbc.weighty = 0.0
        rightPanel.add(buildStatusBar(), gbc)

        gbc.weightx = 1.0
        gbc.weighty = 1.0
        gbc.gridx = 0
        gbc.gridy = 1
        gbc.fill = GridBagConstraints.BOTH

        rightPanel.add(JPanel().apply {
            add(viewport)
        }, gbc)
    }

}