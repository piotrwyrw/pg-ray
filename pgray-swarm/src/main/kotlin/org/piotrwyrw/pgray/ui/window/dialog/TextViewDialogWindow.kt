/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.window.dialog

import com.formdev.flatlaf.util.SystemInfo
import org.piotrwyrw.pgray.apply
import org.piotrwyrw.pgray.let
import org.piotrwyrw.pgray.ui.*
import org.piotrwyrw.pgray.ui.component.BlankCaret
import org.piotrwyrw.pgray.ui.theming.Theme
import org.piotrwyrw.pgray.ui.window.BaseWindow
import java.awt.Dimension
import java.awt.Font
import java.awt.GridBagLayout
import javax.swing.*
import kotlin.let

class TextViewDialogWindow(
    parentFrame: JFrame? = null,
    dialogTitle: String,
    text: String,
    val dialogWidth: Int = 800,
    val dialogHeight: Int = 700,
) : JDialog(parentFrame, dialogTitle, true) {

    private val wrapper = JPanel() apply {
        layout = GridBagLayout()
        isOpaque = false
    }

    private val buttonWrapper = JPanel() apply {
        layout = GridBagLayout()
        isOpaque = false
    }

    private val titleLabel = JLabel(dialogTitle) apply {
        font = font.deriveFont(Font.BOLD, Theme.Text.titleLabelFontSize)
    }

    private val textArea = JTextArea(text) apply {
        font = font.deriveFont(Theme.Text.textAreaFontSize)
        isEditable = false
        background = Theme.Surface.layer4
        caret = BlankCaret()
        caretPosition = 0
    }

    private val textAreaScrollPane = JScrollPane(textArea)

    private val okButton = JButton("Ok") apply {
        background = Theme.Accent.accentColor
        addActionListener { this@TextViewDialogWindow.dispose() }
    }

    fun create() {
        size = Dimension(dialogWidth, dialogHeight)
        defaultCloseOperation = DISPOSE_ON_CLOSE
        layout = GridBagLayout()

        build()

        BaseWindow.applyMacOsFeatures(rootPane)

        setLocationRelativeTo(null)
        isVisible = true
    }

    private fun build() {
        gbc().fillHorizontal let { gbc ->
            wrapper.add(titleLabel, gbc)
        }

        gbc(0, 1) { insets = Insets(20, 0, 20, 0) }.fillBoth let { gbc ->
            wrapper.add(textAreaScrollPane, gbc)
        }

        gbc(0, 2).fillHorizontal let { gbc ->
            wrapper.add(buttonWrapper, gbc)
        }

        buttonWrapper.emptyPanel()

        gbc(1, 0) { ipadx = 10; ipady = 5; } let { gbc ->
            buttonWrapper.add(okButton, gbc)
        }

        gbc().fillBoth.lgInsets let { gbc ->
            add(wrapper, gbc)
        }

        gbc().fillBoth let { gbc ->
            if (SystemInfo.isMacOS) {
                add(JPanel().apply {
                    layout = GridBagLayout()
                    gbc { insets = Insets(40, 20, 20, 20) }.fillBoth let { gbc ->
                        add(wrapper, gbc)
                    }
                }, gbc)
            } else {
                add(wrapper, gbc)
            }
        }
    }

}