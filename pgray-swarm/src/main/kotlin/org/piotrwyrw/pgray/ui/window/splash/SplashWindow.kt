/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.window.splash

import org.piotrwyrw.pgray.render.contract.IOrchestrator
import org.piotrwyrw.pgray.ui.component.CaptionedProgressBar
import org.piotrwyrw.pgray.ui.component.ParticleSystemPanel
import org.piotrwyrw.pgray.ui.dialog.DialogType
import org.piotrwyrw.pgray.ui.dialog.MessageDialog
import org.piotrwyrw.pgray.ui.dialog.option
import org.piotrwyrw.pgray.ui.fillBoth
import org.piotrwyrw.pgray.ui.fillHorizontal
import org.piotrwyrw.pgray.ui.gbc
import org.piotrwyrw.pgray.ui.theming.Theme
import java.awt.Dimension
import java.awt.GridBagLayout
import java.util.concurrent.Executors
import javax.swing.JFrame
import javax.swing.JLabel
import javax.swing.SwingUtilities
import javax.swing.Timer

class SplashWindow(
    val orchestrator: IOrchestrator,
    onComplete: (splash: SplashWindow) -> Unit
) : JFrame("") {

    companion object {
        const val INITIAL_WIDTH = 800
        const val INITIAL_HEIGHT = 400
    }

    private val title = JLabel("Swarm").apply {
        horizontalAlignment = JLabel.CENTER
        font = font.deriveFont(60f)
    }

    private val progressBar = CaptionedProgressBar(0f, "", { onComplete(this) }).apply {
        background = Theme.Accent.accent5
        foreground = Theme.Accent.accentColor
    }

    private val particleSystem = ParticleSystemPanel()

    private val initExecutor = Executors.newSingleThreadExecutor()

    fun create() {
        layout = GridBagLayout()
        size = Dimension(INITIAL_WIDTH, INITIAL_HEIGHT)

        build()

        setLocationRelativeTo(null)
        isVisible = true

        Timer(10) {
            particleSystem.update()
            particleSystem.repaint()
        }.start()

        initExecutor.submit(::initialize)
    }

    private fun build() {
        isUndecorated = true

        gbc(0, 0).fillBoth.let { gbc ->
            add(particleSystem.apply {
                layout = GridBagLayout()
                add(title, gbc())
            }, gbc)
        }

        gbc(0, 1).fillHorizontal.let { gbc ->
            add(progressBar, gbc)
        }
    }

    private fun setStatus(text: String, progress: Float) {
        progressBar.update(text, progress)
    }

    private fun initialize() {
        setStatus("Starting", 0f)
        Thread.sleep(500)
        setStatus("Testing docker connectivity", 10f)
        Thread.sleep(1000)
        orchestrator.getDockerManager().pingDockerServer(ok = {
            setStatus("Starting worker inspection thread", 50f)
            orchestrator.startInspectionThread()
            Thread.sleep(500)
            setStatus("Done.", 99.9f)
            Thread.sleep(1000)
            setStatus(progressBar.caption, 100f)
        }, error = {
            initExecutor.shutdownNow()
            SwingUtilities.invokeLater {
                MessageDialog.show(
                    DialogType.ERROR,
                    "Docker Unavailable",
                    "Could not connect to the docker server. Please start it and try again.",
                    this,
                    option {
                        label { "Close" }
                        highlight { true }
                        onClick { System.exit(0) }
                    }
                )
            }
        })
    }
}