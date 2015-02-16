package offheap
package internal

import C._
import Unsafe.unsafe

class PagePool {
  protected val chunks = AddrStack.alloc(startingSize = 4)
  protected val pages  = AddrStack.alloc(startingSize = 1024)

  override protected def finalize: Unit = {
    while (AddrStack.nonEmpty(chunks))
      unsafe.freeMemory(AddrStack.pop(chunks))
    AddrStack.free(chunks)
    AddrStack.free(pages)
  }

  protected def claimChunks: Unit = {
    val chunk = unsafe.allocateMemory(CHUNK_SIZE)
    AddrStack.push(chunks, chunk)
    var i = 0
    while (i < CHUNK_SIZE / PAGE_SIZE) {
      AddrStack.push(pages, chunk + i * PAGE_SIZE)
      i += 1
    }
  }

  def claim: Addr = {
    if (AddrStack.isEmpty(pages)) claimChunks
    AddrStack.pop(pages)
  }

  def reclaim(page: Addr): Unit = {
    AddrStack.push(pages, page)
  }

  def reclaimStack(otherPages: Ptr[AddrStack]) = this.synchronized {
    AddrStack.merge(pages, otherPages)
  }
}

object PagePool {
  val currentPool = new ThreadLocal[PagePool] {
    override protected def initialValue(): PagePool = new PagePool
  }
}
