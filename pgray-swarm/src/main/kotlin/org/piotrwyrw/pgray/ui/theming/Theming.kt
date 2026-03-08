/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.theming

import com.github.weisj.darklaf.LafManager
import com.github.weisj.darklaf.theme.Theme
import org.piotrwyrw.pgray.brightness
import org.piotrwyrw.pgray.invoke
import java.awt.Color
import javax.swing.SwingUtilities
import javax.swing.UIManager
import kotlin.reflect.KProperty

enum class ThemeMode {
    LIGHT, DARK
}

object ThemeSetup {
    var mode: ThemeMode = ThemeMode.LIGHT
}

private class ThemedColor(light: Color, dark: Color) {
    private val _lightThemeColor = light
    private val _darkThemeColor = dark

    constructor(light: String, dark: String) : this(light(), dark())

    operator fun getValue(thisRef: Any?, property: KProperty<*>): Color = when (ThemeSetup.mode) {
        ThemeMode.LIGHT -> _lightThemeColor
        ThemeMode.DARK -> _darkThemeColor
    }

    operator fun setValue(thisRef: Any?, property: KProperty<*>, value: Color): Nothing =
        throw IllegalStateException("Cannot reassign a ThemedColor")
}

object ThemeColors {
    class Surface {
        val GROUND: Color
            get() = UIManager.getColor("background") ?: Color.BLACK

        val ELEVATION1 get() = GROUND.brightness(1.5)
        val ELEVATION2 get() = GROUND.brightness(2.0)
        val ELEVATION3 get() = GROUND.brightness(2.5)
        val ELEVATION4 get() = GROUND.brightness(3.0)
        val ELEVATION5 get() = GROUND.brightness(3.5)
        val ELEVATION6 get() = GROUND.brightness(4.0)
        val ELEVATION7 get() = GROUND.brightness(4.5)
        val ELEVATION8 get() = GROUND.brightness(5.0)
        val ELEVATION9 get() = GROUND.brightness(5.5)

        val LAYER1 get() = GROUND.brightness(0.9)
        val LAYER2 get() = GROUND.brightness(0.8)
        val LAYER3 get() = GROUND.brightness(0.7)
        val LAYER4 get() = GROUND.brightness(0.6)
        val LAYER5 get() = GROUND.brightness(0.5)
        val LAYER6 get() = GROUND.brightness(0.4)
        val LAYER7 get() = GROUND.brightness(0.3)
        val LAYER8 get() = GROUND.brightness(0.2)
        val LAYER9 get() = GROUND.brightness(0.1)
    }

    val surface = Surface()

    class ContainerStatus {
        val CONTAINER_ABSENT get() = surface.LAYER5
        val CONTAINER_STOPPED by ThemedColor(light = "#e69b93", dark = "#e74c3c")
        val CONTAINER_RUNNING by ThemedColor(light = "#81d6a5", dark = "#2ecc71")
    }

    class ContainerHealth {
        val UNDEFINED get() = surface.LAYER5
        val STARTING by ThemedColor(light = "#edd679", dark = "#f39c12")
        val HEALTHY by ThemedColor(light = "#81d6a5", dark = "#2ecc71")
        val UNHEALTHY by ThemedColor(light = "#e69b93", dark = "#c0392b")
    }

    class Text {
        val FOREGROUND by ThemedColor(
            light = Color.black,
            dark = Color.white
        )
    }

    val accent by ThemedColor(light = "#ff0040", dark = "#c90032")

    val containerStatus = ContainerStatus()
    val containerHealth = ContainerHealth()
    val text = Text()
}

fun useTheme(mode: ThemeMode, then: () -> Unit) {
    ThemeSetup.mode = mode

    val theme: Theme = when (mode) {
        ThemeMode.LIGHT -> IntellijThemeWithAccent(ThemeColors.accent)
        ThemeMode.DARK -> OneDarkThemeWithAccent(ThemeColors.accent)
    }

    SwingUtilities.invokeLater {
        LafManager.setDecorationsEnabled(true)
        LafManager.install(theme)
        then()
    }
}
