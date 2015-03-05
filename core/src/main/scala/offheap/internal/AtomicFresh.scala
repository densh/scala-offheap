package offheap
package internal

import java.util.concurrent.atomic._

class AtomicFresh {
  private val last = new AtomicInteger(0)
  def next = last.incrementAndGet()
}
