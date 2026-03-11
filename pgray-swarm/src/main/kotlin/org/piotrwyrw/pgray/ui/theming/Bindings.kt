/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.theming

import java.awt.Container
import java.util.*
import javax.swing.JComponent

data class PropertyBinding<P>(
    val setter: JComponent.(P) -> Unit,
    val source: () -> P
)

object PropertyBinder {
    private val propertyBindings = mutableListOf<Pair<JComponent, String>>()

    private val JComponent.registeredBindings: List<String>
        get() = propertyBindings.filter { it.first == this }.map { it.second }

    private val Container.descendants: Sequence<JComponent>
        get() = sequence {
            components.forEach { component ->
                if (component is JComponent) yield(component)
                if (component is Container) yieldAll(component.descendants)
            }
        }

    private fun registerBinding(component: JComponent, key: String) {
        propertyBindings.removeAll { it.first == component && it.second == key }
        propertyBindings.add(component to key)
    }

    fun <T : JComponent, P> T.bind(setter: JComponent.(P) -> Unit, source: () -> P) {
        val bindingId = "swarm-${UUID.randomUUID()}"
        putClientProperty(bindingId, PropertyBinding(setter, source))
        registerBinding(this, bindingId)
        setter(source())
    }

    fun updateAllBindings(root: Container) {
        for (comp in root.descendants) {
            for (key in comp.registeredBindings) {
                @Suppress("UNCHECKED_CAST")
                val binding = comp.getClientProperty(key) as? PropertyBinding<Any> ?: continue
                binding.setter(comp, binding.source())
            }
        }
    }
}