/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.theming

import com.formdev.flatlaf.FlatLaf
import com.formdev.flatlaf.intellijthemes.FlatGrayIJTheme
import com.formdev.flatlaf.intellijthemes.FlatOneDarkIJTheme
import com.formdev.flatlaf.util.SystemInfo
import org.piotrwyrw.pgray.brightness
import org.piotrwyrw.pgray.invoke
import java.awt.Color
import java.awt.image.BufferedImage
import javax.swing.ImageIcon
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
    class Surface {
        val ground: Color
            get() = UIManager.getColor("Panel.background") ?: Color.BLACK

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
    }

    val surface = Surface()

    class ContainerStatus {
        val absent get() = surface.layer5
        val stopped by ThemedColor(light = "#e69b93", dark = "#e74c3c")
        val running by ThemedColor(light = "#81d6a5", dark = "#2ecc71")
    }

    class ContainerHealth {
        val undefined get() = surface.layer5
        val starting by ThemedColor(light = "#edd679", dark = "#f39c12")
        val healthy by ThemedColor(light = "#81d6a5", dark = "#2ecc71")
        val unhealthy by ThemedColor(light = "#e69b93", dark = "#c0392b")
    }

    class Icon {
        private val empty = ImageIcon(BufferedImage(1, 1, BufferedImage.TYPE_INT_RGB))
        val error get() = UIManager.getIcon("OptionPane.errorIcon") ?: empty
        val info get() = UIManager.getIcon("OptionPane.informationIcon") ?: empty
        val warning get() = UIManager.getIcon("OptionPane.warningIcon") ?: empty
    }

    class Text {
        val foreground by ThemedColor(
            light = Color.black,
            dark = Color.white
        )

        val titleLabelFontSize = 16f
        val textAreaFontSize = 14f
    }

    class Accent {
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

        val titleBarColor by ThemedColor(light = { accentColor.brightness(1.3) }, dark = { accent2 })
    }

    val containerStatus = ContainerStatus()
    val containerHealth = ContainerHealth()
    val icon = Icon()
    val text = Text()
    val accent = Accent()
}

fun useTheme(mode: ThemeMode, then: () -> Unit) {
    ThemeSetup.mode = mode

    if (SystemInfo.isMacOS) {
        System.setProperty("apple.awt.application.appearance", mode.macApplicationAppearance)
        System.setProperty("apple.awt.application.name", "Swarm");
        System.setProperty("apple.laf.useScreenMenuBar", "true")
    }

    val laf = when (mode) {
        ThemeMode.LIGHT -> FlatGrayIJTheme()
        ThemeMode.DARK -> FlatOneDarkIJTheme()
    }

    val accentColor = laf.defaults.getColor("Component.accentColor")
    Theme.accent.accentColor = accentColor

    SwingUtilities.invokeLater {
        FlatLaf.setup(laf)
        then()
    }
}