/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.theming

import org.piotrwyrw.pgray.let
import org.slf4j.LoggerFactory
import java.awt.Container
import java.util.concurrent.atomic.AtomicInteger
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

    private val registeredBindingCount = AtomicInteger(0)

    private val log = LoggerFactory.getLogger(javaClass)

    private fun nextBindingName(component: JComponent): String =
        "swarm-${component.javaClass.simpleName.lowercase()}-binding-${registeredBindingCount.incrementAndGet()}"

    private fun registerBinding(component: JComponent, key: String) {
        propertyBindings.removeAll { it.first == component && it.second == key }
        propertyBindings.add(component to key)
    }

    fun <T : JComponent, P> T.bind(setter: JComponent.(P) -> Unit, source: () -> P) {
        val bindingId = nextBindingName(this)
        putClientProperty(bindingId, PropertyBinding(setter, source))
        registerBinding(this, bindingId)
        setter(source())
        log.debug("Binding $bindingId created for ${this.javaClass.simpleName}")
    }

    @Suppress("UNCHECKED_CAST")
    fun updateAllBindings(root: Container) {
        for (comp in root.descendants) {
            for (key in comp.registeredBindings) {
                (comp.getClientProperty(key) as? PropertyBinding<Any>)?.let {
                    it.setter(comp, it.source())
                    log.debug("Updated binding $key for ${comp.javaClass.simpleName}")
                }
            }
        }
    }
}