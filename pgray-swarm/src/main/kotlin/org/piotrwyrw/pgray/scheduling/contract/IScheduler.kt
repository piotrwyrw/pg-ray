/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.scheduling.contract

interface IScheduler {
    fun <R> submit(
        task: () -> R,
        onSuccess: (R, scheduleAgain: () -> Unit) -> Unit,
        onError: (Throwable, retry: () -> Unit) -> Unit = { _, _ -> },
        priority: Int = 0
    )

    fun shutdown()
}