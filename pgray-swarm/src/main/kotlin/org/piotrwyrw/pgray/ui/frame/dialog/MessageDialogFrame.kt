/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.frame.dialog

import org.piotrwyrw.pgray.apply
import org.piotrwyrw.pgray.ui.Insets
import org.piotrwyrw.pgray.ui.fillBoth
import org.piotrwyrw.pgray.ui.fillHorizontal
import org.piotrwyrw.pgray.ui.frame.BaseFrame
import org.piotrwyrw.pgray.ui.gbc
import org.piotrwyrw.pgray.ui.theming.Theme
import java.awt.Font
import java.awt.GridBagLayout
import javax.swing.JButton
import javax.swing.JFrame
import javax.swing.JLabel
import javax.swing.JPanel

typealias DialogActionFn = (dialog: MessageDialogFrame) -> Unit

class MessageDialogFrame(
    val parentFrame: JFrame?,
    dialogTitle: String,
    val message: String,
    val dialogType: DialogType,
    val options: MutableList<DialogOption> = mutableListOf()
) : BaseFrame(
    frameTitle = dialogTitle,
    initialWidth = 400,
    initialHeight = 200,
    _resizable = true,
    closeOperator = DISPOSE_ON_CLOSE
) {
    companion object {
        const val WIDTH = 400
    }

    private val backgroundColor = Theme.surface.layer4

    private val icon = when (dialogType) {
        DialogType.INFO -> Theme.icon.info
        DialogType.WARNING -> Theme.icon.warning
        DialogType.ERROR -> Theme.icon.error
    }

    private val iconLabel = JLabel() apply {
        icon = this@MessageDialogFrame.icon
        isOpaque = false
    }

    private val titleLabel = JLabel(dialogTitle) apply { font = font.deriveFont(Font.BOLD, 15f) }

    private val messageLabel = JLabel("<html><div style=\"width: ${WIDTH}px\">$message</div></html>")

    private val wrapper = JPanel() apply {
        layout = GridBagLayout()
        isOpaque = false
    }

    private val westButtonSpacer = JPanel() apply {
        isOpaque = false
    }

    private val buttonWrapper = JPanel() apply {
        layout = GridBagLayout()
        isOpaque = false
    }

    private var highlightedButton: JButton? = null

    override fun buildUI() {
        parentFrame?.isEnabled = false

        if (isAlwaysOnTopSupported) {
            isAlwaysOnTop = true
        }

        if (options.isEmpty()) {
            options.add(DialogOption.default())
        }

        //Dialog Icon
        gbc(0, 0) {
            gridheight = 2
            gridwidth = 1
            insets = Insets(0, 0, 0, 20)
        }.let { gbc ->
            wrapper.add(iconLabel, gbc)
        }

        // Dialog Title
        gbc(1, 0).fillHorizontal.let { gbc ->
            wrapper.add(titleLabel, gbc)
        }

        // Dialog Message
        gbc(1, 1) { ipady = 10 }.fillHorizontal.let { gbc ->
            wrapper.add(messageLabel, gbc)
        }

        // Spacer between the dialog message and the button wrapper
        gbc(0, 2) { gridwidth = 2 }.fillBoth.let { gbc ->
            wrapper.add(JPanel() apply { isOpaque = false }, gbc)
        }

        // Button Wrapper
        gbc(0, 3) {
            insets = Insets(10, 0, 0, 0)
            gridwidth = 2
        }.fillHorizontal.let { gbc ->
            wrapper.add(buttonWrapper, gbc)
        }


        // Buttons West Spacer
        gbc(0, 0).fillBoth.let { gbc ->
            buttonWrapper.add(westButtonSpacer, gbc)
        }

        // Button
        options.forEachIndexed { index, option ->
            val btn = JButton(option.label)

            if (option.highlighted) {
                if (highlightedButton != null) {
                    throw IllegalStateException("Only one button can be marked as highlighted. Failed on \"${option.label}\", previously highlighted \"${highlightedButton!!.text}\"")
                }

                btn.background = Theme.accent.accentColor
                btn.font = btn.font.deriveFont(Font.BOLD)
                this.highlightedButton = btn
            }

            btn.addActionListener { _ ->
                option.action(this@MessageDialogFrame)
            }

            btn.isOpaque = false

            gbc(1 + index, 0) {
                ipady = 5;
                insets = Insets { left = 5 }
            }.let { gbc ->
                buttonWrapper.add(btn, gbc)
            }
        }

        // The wrapper itself
        gbc { insets = Insets(20) }.fillBoth.let { gbc ->
            add(wrapper, gbc)
        }

        contentPane.background = backgroundColor

        pack()
    }

    override fun afterFrameShown() {
        this.highlightedButton?.requestFocusInWindow()
    }

    override fun onWindowClosing() {
        parentFrame?.isEnabled = true
    }
}