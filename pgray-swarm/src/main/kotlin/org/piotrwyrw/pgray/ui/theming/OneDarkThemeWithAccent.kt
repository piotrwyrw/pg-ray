/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.theming

import com.github.weisj.darklaf.theme.OneDarkTheme
import com.github.weisj.darklaf.theme.spec.AccentColorRule
import java.awt.Color

class OneDarkThemeWithAccent(val accent: Color) : OneDarkTheme() {
    override fun getAccentColorRule(): AccentColorRule = AccentColorRule(accent)
}