package offheap
package internal

import Unsafe.unsafe

class PagePool {
  private val chunks = new AddrStack(startingSize = 4, growthFactor = 2)
  private val pages  = new AddrStack(startingSize = 1024, growthFactor = 2)
  private def claimChunks: Unit = {
    val chunk = unsafe.allocateMemory(CHUNK_SIZE)
    chunks.push(chunk)
    var i = 0
    while (i < CHUNK_SIZE / PAGE_SIZE) {
      pages.push(chunk + i * PAGE_SIZE)
      i += 1
    }
  }
  override protected def finalize: Unit = {
    while (chunks.nonEmpty)
      unsafe.freeMemory(chunks.pop)
    chunks.dispose
    pages.dispose
  }
  def claim: Addr = {
    if (pages.isEmpty) claimChunks
    pages.pop
  }
  def reclaim(page: Addr): Unit =
    pages.push(page)
  def reclaim(otherPages: AddrStack) =
    pages.merge(otherPages)
}

object PagePool {
  val currentPool = new ThreadLocal[PagePool] {
    override protected def initialValue(): PagePool = new PagePool
  }
}
