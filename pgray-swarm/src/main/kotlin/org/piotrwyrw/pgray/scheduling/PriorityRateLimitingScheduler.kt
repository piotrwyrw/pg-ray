/*
 * Copyright (c) 2026 Piotr Krzysztof Wyrwas [pg-ray]
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

package org.piotrwyrw.pgray.scheduling

import org.piotrwyrw.pgray.component1
import org.piotrwyrw.pgray.component2
import org.piotrwyrw.pgray.scheduling.contract.ISchedulable
import org.piotrwyrw.pgray.scheduling.contract.IScheduler
import org.slf4j.LoggerFactory
import java.time.Duration
import java.util.concurrent.*
import java.util.concurrent.atomic.AtomicBoolean

/**
 * A scheduler implementation that limits the number of simultaneous tasks
 */
class PriorityRateLimitingScheduler(maxSimultaneousTaskCount: Int = DEFAULT_MAX_SIMULTANEOUS_TASKS) : IScheduler {

    companion object {
        val DEFAULT_MAX_SIMULTANEOUS_TASKS = 10
        private val TASK_EXECUTOR_SHUTDOWN_TIMEOUT = Duration.ofSeconds(20)
        private val SCHEDULING_EXECUTOR_SHUTDOWN_TIMEOUT = Duration.ofSeconds(20)
    }

    private val log = LoggerFactory.getLogger(javaClass)

    private var _taskExecutor: ThreadPoolExecutor? = null
    private val taskExecutor: ThreadPoolExecutor
        get() {
            return _taskExecutor ?: run {
                _taskExecutor = Executors.newCachedThreadPool() as ThreadPoolExecutor
                _taskExecutor!!
            }
        }

    private var _schedulingExecutor: ScheduledExecutorService? = null
    private val schedulingExecutor: ScheduledExecutorService
        get() {
            return _schedulingExecutor ?: run {
                _schedulingExecutor = Executors.newSingleThreadScheduledExecutor()
                _schedulingExecutor!!
            }
        }

    private val schedulerRunning = AtomicBoolean(false)

    private val scheduledTasks = PriorityBlockingQueue<ISchedulable>(
        maxSimultaneousTaskCount,
        compareByDescending { it.priority() }
    )

    private val taskSemaphore = Semaphore(maxSimultaneousTaskCount)

    private fun startScheduler() {
        if (schedulerRunning.get())
            return

        schedulerRunning.set(true)

        val stackTrace = Throwable().stackTrace[2]
        val trigger = "${stackTrace.className}.${stackTrace.methodName} [${stackTrace.fileName}:${stackTrace.lineNumber}]"

        log.info("Starting priority rate limiting scheduler")
        log.info("Scheduler startup triggered by: $trigger")
        schedulingExecutor.scheduleAtFixedRate({
            scheduleNextTasks()
        }, 0, 100, TimeUnit.MILLISECONDS)
    }

    private fun scheduleNextTasks() {
        while (true) {
            val task = scheduledTasks.poll() ?: break

            if (!taskSemaphore.tryAcquire()) {
                scheduledTasks.add(task)
                break
            }

            taskExecutor.submit {
                try {
                    task.invokeOnSuccess(task.invokeTask()) {
                        submit(task)
                    }
                } catch (e: Throwable) {
                    task.invokeOnError(e) {
                        submit(task)
                    }
                } finally {
                    taskSemaphore.release()
                }
            }
        }
    }

    private fun submit(task: ISchedulable) {
        scheduledTasks.put(task)
    }

    override fun <R> submit(
        task: () -> R,
        onSuccess: (R, scheduleAgain: () -> Unit) -> Unit,
        onError: (Throwable, retry: () -> Unit) -> Unit,
        priority: Int
    ) {
        if (!schedulerRunning.get())
            startScheduler()

        scheduledTasks.put(ScheduledTask(task, onSuccess, onError, priority))
    }

    override fun shutdown() {
        val taskExecutor = this.taskExecutor
        val schedulingExecutor = this.schedulingExecutor

        log.info("Shutting down task executor ...")
        val (executorTimeout, executorTimeoutUnit) = TASK_EXECUTOR_SHUTDOWN_TIMEOUT
        taskExecutor.shutdown()
        taskExecutor.awaitTermination(executorTimeout, executorTimeoutUnit)
        log.info("Task executor shut down.")

        log.info("Shutting down scheduling executor ...")
        val (schedulerTimeout, schedulerTimeoutUnit) = SCHEDULING_EXECUTOR_SHUTDOWN_TIMEOUT
        schedulingExecutor.shutdown()
        schedulingExecutor.awaitTermination(schedulerTimeout, schedulerTimeoutUnit)
        log.info("Scheduling executor shut down.")

        _taskExecutor = null
        _schedulingExecutor = null
        schedulerRunning.set(false)
    }
}