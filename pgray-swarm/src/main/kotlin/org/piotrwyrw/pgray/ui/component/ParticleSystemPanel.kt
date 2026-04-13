/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.ui.component

import org.piotrwyrw.pgray.ui.theming.Theme
import java.awt.*
import java.awt.geom.Path2D
import javax.swing.JPanel
import kotlin.math.PI
import kotlin.math.pow
import kotlin.math.sin
import kotlin.math.sqrt
import kotlin.random.Random

data class Vec2(var x: Double, var y: Double) {
    constructor(x: Number, y: Number) : this(x.toDouble(), y.toDouble())

    constructor() : this(0.0, 0.0)

    constructor(
        randomBoundsX: ClosedFloatingPointRange<Double>,
        randomBoundsY: ClosedFloatingPointRange<Double>
    ) : this(
        x = if (randomBoundsX.start == randomBoundsX.endInclusive) 0.0 else Random.nextDouble(
            randomBoundsX.start,
            randomBoundsX.endInclusive
        ),
        y = if (randomBoundsY.start == randomBoundsY.endInclusive) 0.0 else Random.nextDouble(
            randomBoundsY.start,
            randomBoundsY.endInclusive
        )
    )

    fun copy() = Vec2(x, y)

    fun lengthSquared() = x * x + y * y
    fun length() = sqrt(lengthSquared())

    fun set(other: Vec2) = apply {
        x = other.x
        y = other.y
    }

    fun multiply(fac: Number) = apply {
        x *= fac.toDouble()
        y *= fac.toDouble()
    }

    operator fun times(fac: Number) = copy().multiply(fac)

    operator fun timesAssign(fac: Number) {
        multiply(fac)
    }

    fun divide(d: Number) = apply {
        x /= d.toDouble()
        y /= d.toDouble()
    }

    operator fun div(d: Number) = copy().divide(d)

    operator fun divAssign(d: Number) {
        divide(d)
    }

    fun add(other: Vec2) = apply {
        x += other.x
        y += other.y
    }

    operator fun plus(other: Vec2) = copy().add(other)

    operator fun plusAssign(other: Vec2) {
        add(other)
    }

    fun subtract(other: Vec2) = apply {
        x -= other.x
        y -= other.y
    }

    operator fun minus(other: Vec2) = copy().subtract(other)

    operator fun minusAssign(other: Vec2) {
        subtract(other)
    }

    fun flip() = apply {
        x = -x
        y = -y
    }

    operator fun not() = copy().flip()

    fun limit(maxValue: Number): Vec2 {
        val max = maxValue.toDouble()
        if (length() <= max)
            return this
        normalize()
        multiply(max)
        return this
    }

    operator fun get(maxValue: Number) = copy().limit(maxValue)

    fun normalize() = apply {
        val lenSq = lengthSquared()
        if (lenSq == 0.0) return@apply
        divide(sqrt(lenSq))
    }
}

data class Particle(val location: Vec2, val velocity: Vec2, val acceleration: Vec2, val bounds: Rectangle) {
    private val maxLength: Int = 10
    private val previousLocations = mutableListOf<Vec2>()

    private var noise = 0.0
    private val collisionNoise = 0.5

    private var frames = 0

    constructor(location: Vec2, bounds: Rectangle) : this(location, Vec2(), Vec2(), bounds)

    val tailPoint: Vec2?
        get() = previousLocations.getOrNull(0)

    fun update(attractor: Vec2) {
//        acceleration.set(((attractor - location) / (attractor - location).length()))
        val direction = (attractor - location)

        acceleration += (direction.copy() / direction.lengthSquared())
        acceleration.limit(0.15)

        frames ++
        val angle = (frames.toDouble() / 200.0) * 2 * PI

        noise = ((sin(angle) + 1) / 2.0) * 0.05 + 0.02

        velocity += acceleration.copy() + Vec2(0.0..noise, 0.0..noise) * if (Random.nextBoolean()) -1 else 1
        velocity.limit(4.2)

        val newLocation = location + velocity
        if (!bounds.contains(newLocation.x, newLocation.y)) {
            velocity.flip()
            velocity.add(Vec2(0.0..collisionNoise, 0.0..collisionNoise))
        }

        location += velocity

        previousLocations.add(location.copy())

        if (previousLocations.size > maxLength) {
            previousLocations.removeAt(0)
        }
    }

    fun paint(g: Graphics2D, color: Color) {
//        g.color = color
//        g.fillRect(location.x.toInt(), location.y.toInt(), 1, 1)

        val path = Path2D.Double()

        previousLocations.firstOrNull()?.let { first ->
            path.moveTo(first.x, first.y)
        }

        previousLocations.drop(1).forEach { point ->
            path.lineTo(point.x, point.y)
        }

//        val alpha = (1.0 - (previousLocations.size / maxLength.toDouble())) * 255
        g.color = Color(color.red, color.green, color.blue, 20)
        g.stroke = BasicStroke(3f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND)
        g.draw(path)
    }
}

class ParticleSystemPanel : JPanel() {
    private val particles by lazy {
        val list = mutableListOf<Particle>()
        repeat(1000) {
            list.add(Particle(Vec2(0.0..width.toDouble(), 0.0..height.toDouble()), Rectangle(0, 0, width, height)))
        }
        list
    }

    private val angleDelta = 0.025
    private var angle = 0.0

    fun update() {
//        repeat(1) {
//            particles.add(Particle(Vec2(0.0..width.toDouble(), 0.0..height.toDouble())))
//        }

        val size = 20

//        val firstAttractor = Vec2(width / 2 + cos(angle) * size, height / 2 + sin(angle) * size)
//        val secondAttractor = Vec2(width / 2 + cos(angle + PI) * size, height / 2 + sin(angle + PI) * size)
        val attractor = Vec2(width / 2, height / 2)
        angle += angleDelta

        particles.forEach {
            it.update(attractor)
        }
//        particles.removeAll { particle ->
//            val tail = particle.tailPoint ?: return@removeAll false
//            tail.x > width || tail.y > height
//        }
    }

    override fun paintComponent(g: Graphics) {
        val g2d = g as Graphics2D
        g2d.composite = AlphaComposite.SrcOver

        g2d.color = Theme.Surface.layer5
        g2d.fillRect(0, 0, width, height)

        particles.forEach { particle ->
            particle.paint(g, Theme.Accent.accentColor)
        }
    }
}