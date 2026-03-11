/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.window.dialog

import org.piotrwyrw.pgray.apply
import org.piotrwyrw.pgray.let
import org.piotrwyrw.pgray.ui.*
import org.piotrwyrw.pgray.ui.dialog.DialogOption
import org.piotrwyrw.pgray.ui.dialog.DialogType
import org.piotrwyrw.pgray.ui.theming.Theme
import java.awt.Dimension
import java.awt.Font
import java.awt.GridBagLayout
import javax.swing.*

typealias DialogActionFn = (dialog: MessageDialogWindow) -> Unit

class MessageDialogWindow(
    parentFrame: JFrame,
    dialogTitle: String,
    message: String,
    dialogType: DialogType,
    val options: MutableList<DialogOption> = mutableListOf()
) : JDialog(parentFrame, dialogTitle, true) {
    companion object {
        const val TEXT_WIDTH = 400
    }

    private val backgroundColor = Theme.surface.layer4

    private val icon = when (dialogType) {
        DialogType.INFO -> Theme.icon.info
        DialogType.WARNING -> Theme.icon.warning
        DialogType.ERROR -> Theme.icon.error
    }

    private val iconLabel = JLabel() apply {
        icon = this@MessageDialogWindow.icon
        isOpaque = false
    }

    private val titleLabel =
        JLabel(dialogTitle) apply { font = font.deriveFont(Font.BOLD, Theme.text.titleLabelFontSize) }

    private val messageLabel = JLabel("<html><div style=\"width: ${TEXT_WIDTH}px\">$message</div></html>")

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

    fun create() {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        layout = GridBagLayout()

        build()
        contentPane.background = backgroundColor

        pack()

        val size = this.size
        minimumSize = size
        maximumSize = size

        setLocationRelativeTo(null)
        isVisible = true

        SwingUtilities.invokeLater {
            highlightedButton?.requestFocusInWindow()
        }
    }

    private fun build() {
        if (options.isEmpty()) {
            options.add(DialogOption.defaultOption())
        }

        //Dialog Icon
        gbc(0, 0) {
            gridheight = 2
            gridwidth = 1
            insets = Insets(0, 0, 0, 20)
        } let { gbc ->
            wrapper.add(iconLabel, gbc)
        }

        // Dialog Title
        gbc(1, 0).fillHorizontal let { gbc ->
            wrapper.add(titleLabel, gbc)
        }

        // Dialog Message
        gbc(1, 1) { ipady = 10 }.fillHorizontal let { gbc ->
            wrapper.add(messageLabel, gbc)
        }

        // Spacer between the dialog message and the button wrapper
        gbc(0, 2) { gridwidth = 2 }.fillBoth let { gbc ->
            wrapper.add(JPanel() apply { isOpaque = false }, gbc)
        }

        // Button Wrapper
        gbc(0, 3) {
            insets = Insets(10, 0, 0, 0)
            gridwidth = 2
        }.fillHorizontal let { gbc ->
            wrapper.add(buttonWrapper, gbc)
        }


        // Buttons West Spacer
        gbc(0, 0).fillBoth let { gbc ->
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
                option.action(this@MessageDialogWindow)
            }

            gbc(1 + index, 0) {
                ipady = 5;
                insets = Insets { left = 5 }
            } let { gbc ->
                buttonWrapper.add(btn, gbc)
            }
        }

        // The wrapper itself
        contentPane = JPanel().apply {
            layout = GridBagLayout()
            gbc().fillBoth.lgInsets.let { gbc ->
                this.add(wrapper, gbc)
            }
        }
    }
}