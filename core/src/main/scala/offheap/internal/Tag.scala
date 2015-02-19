package offheap
package internal

import java.util.concurrent.atomic.AtomicInteger

object Tag {
  private val last = new AtomicInteger(0)
  def next = last.incrementAndGet()
}
