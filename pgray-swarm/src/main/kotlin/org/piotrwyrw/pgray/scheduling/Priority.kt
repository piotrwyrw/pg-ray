/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.scheduling

object Priority {
    const val LOWEST = -Int.MAX_VALUE
    const val HIGHEST = Int.MAX_VALUE

    const val DEFAULT = 0
    const val WORKER_INSPECT = 1
    const val WORKER_START = 2
    const val WORKER_CREATE = 3
    const val WORKER_REMOVE = 4
    const val WORKER_STOP = 5
}