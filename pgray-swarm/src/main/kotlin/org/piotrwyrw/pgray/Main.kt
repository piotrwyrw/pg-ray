package org.piotrwyrw.pgray

import com.github.weisj.darklaf.LafManager
import com.github.weisj.darklaf.theme.DarculaTheme
import org.piotrwyrw.pgray.ui.SwarmFrame
import javax.swing.SwingUtilities

fun main() {
    SwingUtilities.invokeLater { LafManager.install(DarculaTheme()); SwarmFrame() }
}