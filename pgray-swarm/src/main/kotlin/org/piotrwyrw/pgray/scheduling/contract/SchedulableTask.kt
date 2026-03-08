/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.scheduling.contract

interface SchedulableTask {
    fun invokeTask(): Any?
    fun invokeOnSuccess(parameter: Any?, scheduleAgain: () -> Unit)
    fun invokeOnError(throwable: Throwable, retry: () -> Unit)

    fun priority(): Int
}