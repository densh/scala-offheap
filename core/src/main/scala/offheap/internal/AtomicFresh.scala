package offheap
package internal

import java.util.concurrent.atomic._

class AtomicFreshInt {
  private val last = new AtomicInteger(0)
  def next = last.incrementAndGet()
}

class AtomicFreshLong {
  private val last = new AtomicLong(0L)
  def next = last.incrementAndGet()
}
