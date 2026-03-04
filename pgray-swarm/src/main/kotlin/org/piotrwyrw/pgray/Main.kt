package org.piotrwyrw.pgray

import com.github.weisj.darklaf.LafManager
import com.github.weisj.darklaf.theme.DarculaTheme
import com.github.weisj.darklaf.theme.IntelliJTheme
import com.github.weisj.darklaf.theme.OneDarkTheme
import org.piotrwyrw.pgray.render.RenderingOrchestratorImpl
import org.piotrwyrw.pgray.ui.SwarmFrame
import javax.swing.SwingUtilities

fun main() {
    val orchestrator = RenderingOrchestratorImpl()
    SwingUtilities.invokeLater {
        LafManager.install(OneDarkTheme())
        SwarmFrame(orchestrator)
    }
}