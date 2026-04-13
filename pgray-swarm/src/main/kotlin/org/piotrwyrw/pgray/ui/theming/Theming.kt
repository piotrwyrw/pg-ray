/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.theming

import com.formdev.flatlaf.FlatLaf
import com.formdev.flatlaf.FlatPropertiesLaf
import com.formdev.flatlaf.util.SystemInfo
import com.jthemedetecor.OsThemeDetector
import org.piotrwyrw.pgray.brightness
import org.piotrwyrw.pgray.invoke
import java.awt.Color
import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import javax.swing.JFrame
import javax.swing.SwingUtilities
import javax.swing.UIManager
import kotlin.reflect.KProperty

enum class ThemeMode(val macApplicationAppearance: String) {
    LIGHT("NSAppearanceNameAqua"),
    DARK("NSAppearanceNameDarkAqua")
}

object ThemeSetup {
    var mode: ThemeMode = ThemeMode.LIGHT
}

class ThemedColor(light: () -> Color, dark: () -> Color) {
    private val _lightThemeColor = light
    private val _darkThemeColor = dark

    constructor(light: String, dark: String) : this({ light() }, { dark() })

    constructor(light: Color, dark: Color) : this({ light }, { dark })

    operator fun getValue(thisRef: Any?, property: KProperty<*>): Color = when (ThemeSetup.mode) {
        ThemeMode.LIGHT -> _lightThemeColor()
        ThemeMode.DARK -> _darkThemeColor()
    }

    operator fun setValue(thisRef: Any?, property: KProperty<*>, value: Color): Nothing =
        throw IllegalStateException("Cannot reassign a ThemedColor")
}

object Theme {
    object Surface {
        val ground: Color
            get() = UIManager.getColor("Panel.background") ?: Color.WHITE

        val elevation1 get() = ground.brightness(1.5)
        val elevation2 get() = ground.brightness(2.0)
        val elevation3 get() = ground.brightness(2.5)
        val elevation4 get() = ground.brightness(3.0)
        val elevation5 get() = ground.brightness(3.5)
        val elevation6 get() = ground.brightness(4.0)
        val elevation7 get() = ground.brightness(4.5)
        val elevation8 get() = ground.brightness(5.0)
        val elevation9 get() = ground.brightness(5.5)

        val layer1 get() = ground.brightness(0.9)
        val layer2 get() = ground.brightness(0.8)
        val layer3 get() = ground.brightness(0.7)
        val layer4 get() = ground.brightness(0.6)
        val layer5 get() = ground.brightness(0.5)
        val layer6 get() = ground.brightness(0.4)
        val layer7 get() = ground.brightness(0.3)
        val layer8 get() = ground.brightness(0.2)
        val layer9 get() = ground.brightness(0.1)
        val layer11 get() = ground.brightness(0.09)
        val layer12 get() = ground.brightness(0.08)
        val layer13 get() = ground.brightness(0.07)
        val layer14 get() = ground.brightness(0.06)
        val layer15 get() = ground.brightness(0.05)
        val layer16 get() = ground.brightness(0.04)
        val layer17 get() = ground.brightness(0.03)
        val layer18 get() = ground.brightness(0.02)
        val layer19 get() = ground.brightness(0.01)
    }

    object ContainerStatus {
        val absent by ThemedColor(light = { Surface.layer5 }, dark = { Surface.elevation3 })
        val stopped by ThemedColor(light = "#e69b93", dark = "#e74c3c")
        val running by ThemedColor(light = "#6cd067", dark = "#33a33f")
    }

    object ContainerHealth {
        val undefined by ThemedColor(light = { Surface.layer2 }, dark = { Surface.layer14 })
        val starting by ThemedColor(light = "#edd679", dark = "#f39c12")
        val healthy by ThemedColor(light = "#6cd067", dark = "#33a33f")
        val unhealthy by ThemedColor(light = "#e69b93", dark = "#c0392b")
    }

    object Icon {
        private val empty = ImageIcon(BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB))
        val error get() = UIManager.getIcon("OptionPane.errorIcon") ?: empty
        val info get() = UIManager.getIcon("OptionPane.informationIcon") ?: empty
        val warning get() = UIManager.getIcon("OptionPane.warningIcon") ?: empty
    }

    object Text {
        val foreground by ThemedColor(
            light = Color.black,
            dark = Color.white
        )

        val titleLabelFontSize = 16f
        val textAreaFontSize = 14f
    }

    object Accent {
        var accentColor: Color = Color.black // Fallback color. This will be changed on LAF init

        val accent1 get() = accentColor.brightness(0.9)
        val accent2 get() = accentColor.brightness(0.8)
        val accent3 get() = accentColor.brightness(0.7)
        val accent4 get() = accentColor.brightness(0.6)
        val accent5 get() = accentColor.brightness(0.5)
        val accent6 get() = accentColor.brightness(0.4)
        val accent7 get() = accentColor.brightness(0.3)
        val accent8 get() = accentColor.brightness(0.2)
        val accent9 get() = accentColor.brightness(0.1)
    }

    object TitleBar {
        val titleBarColor: Color get() = UIManager.getColor("TitleBar.background") ?: Color.black
    }

    object StatusBar {
        val statusBarColor by ThemedColor(light = { Surface.layer1 }, dark = { Surface.layer5 })
    }

    object List {
        val selectedBackground: Color get() = UIManager.getColor("List.selectionBackground")
    }
}

object ThemeLoader {
    const val DARK_THEME_PATH = "/themes/FlatSwarmDarkTheme.properties"
    const val LIGHT_THEME_PATH = "/themes/FlatSwarmLightTheme.properties"

    private val darkThemeInputStream = javaClass.getResourceAsStream(DARK_THEME_PATH)
        ?: throw IllegalStateException("Could not load dark theme: ${DARK_THEME_PATH}")

    private val lightThemeInputStream = javaClass.getResourceAsStream(LIGHT_THEME_PATH)
        ?: throw IllegalStateException("Could not load light theme: ${LIGHT_THEME_PATH}")

    val darkTheme by lazy { FlatPropertiesLaf("FlatSwarmDarkTheme", darkThemeInputStream) }
    val lightTheme by lazy { FlatPropertiesLaf("FlatSwarmLightTheme", lightThemeInputStream) }
}

fun useSystemTheme(then: () -> Unit = {}) {
    OsThemeDetector.getDetector().isDark.let { dark ->
        useTheme(if (dark) ThemeMode.DARK else ThemeMode.LIGHT, then)
    }
}

fun useTheme(mode: ThemeMode, then: () -> Unit = {}) {
    ThemeSetup.mode = mode

    if (SystemInfo.isMacOS) {
        System.setProperty("apple.awt.application.appearance", mode.macApplicationAppearance)
        System.setProperty("apple.awt.application.name", "Swarm");
        System.setProperty("apple.laf.useScreenMenuBar", "true")
    }

    val laf = when (mode) {
        ThemeMode.LIGHT -> ThemeLoader.lightTheme
        ThemeMode.DARK -> ThemeLoader.darkTheme
    }

    val accentColor = laf.defaults.getColor("Component.accentColor")
    Theme.Accent.accentColor = accentColor

    SwingUtilities.invokeLater {
        FlatLaf.setup(laf)

        for (window in JFrame.getWindows()) {
            SwingUtilities.updateComponentTreeUI(window)
            PropertyBinder.updateAllBindings(window)
        }

        then()
    }
}