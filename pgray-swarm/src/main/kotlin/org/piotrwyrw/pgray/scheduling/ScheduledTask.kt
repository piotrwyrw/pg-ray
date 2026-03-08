/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.scheduling

import org.piotrwyrw.pgray.scheduling.contract.SchedulableTask

data class ScheduledTask<R>(
    val task: () -> R,
    val onSuccess: (R, scheduleAgain: () -> Unit) -> Unit,
    val onError: (Throwable, retry: () -> Unit) -> Unit,
    val priority: Int,
) : SchedulableTask {
    override fun invokeTask(): R {
        return task()
    }

    override fun invokeOnSuccess(parameter: Any?, scheduleAgain: () -> Unit) {
        onSuccess(parameter as R, scheduleAgain)
    }

    override fun invokeOnError(throwable: Throwable, retry: () -> Unit) {
        onError(throwable, retry)
    }

    override fun priority(): Int = priority
}
