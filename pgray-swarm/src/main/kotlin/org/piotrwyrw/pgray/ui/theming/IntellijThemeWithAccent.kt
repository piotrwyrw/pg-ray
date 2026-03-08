/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.theming

import com.github.weisj.darklaf.theme.IntelliJTheme
import com.github.weisj.darklaf.theme.spec.AccentColorRule
import org.piotrwyrw.pgray.invoke
import java.awt.Color

class IntellijThemeWithAccent(val accent: Color) : IntelliJTheme() {
    override fun getAccentColorRule(): AccentColorRule = AccentColorRule(accent)
}