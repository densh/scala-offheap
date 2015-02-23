package offheap
package internal

import Unsafer.unsafe

class LinkedPagePool {
  private var chunk: LinkedChunk = null
  private var page: LinkedPage = null
  private def allocateChunk(): Unit = {
    val start = unsafe.allocateMemory(CHUNK_SIZE)
    chunk = new LinkedChunk(start, chunk)
    var i = 0
    while (i < CHUNK_SIZE / PAGE_SIZE) {
      page = new LinkedPage(start + i * PAGE_SIZE, 0, page)
      i += 1
    }
  }
  def claim(): LinkedPage = this.synchronized {
    if (page == null) allocateChunk()
    val res = page
    page = res.next
    res.next = null
    res
  }
  def reclaim(head: LinkedPage): Unit = this.synchronized {
    var tail = head
    while (tail.next != null)  tail = tail.next
    tail.next = page
    page = head
  }
}
object LinkedPagePool extends LinkedPagePool
/*{
  private val pools = new Array[LinkedPagePool](PAGE_POOLS)
  def apply(obj: Object): LinkedPagePool = LinkedPagePool.synchronized {
    val idx = obj.hashCode % PAGE_POOLS
    val candidate = pools(idx)
    if (candidate != null) candidate
    else {
      val pool = new LinkedPagePool
      pools(idx) = pool
      pool
    }
  }
}*/

final class LinkedChunk(val start: Long, var next: LinkedChunk)

final class LinkedPage(val start: Long, var offset: Long, var next: LinkedPage)

final class LinkedRegion extends offheap.Region {
  private var page = LinkedPagePool.claim
  def isOpen: Boolean = page != null
  def close(): Unit = this.synchronized {
    LinkedPagePool.reclaim(page)
    page = null
  }
  def allocate(size: Size): Addr = this.synchronized {
    if (!isOpen) throw InaccessibleRegionException
    if (size > PAGE_SIZE) throw new IllegalArgumentException
    val currentOffset = page.offset
    if (currentOffset + size <= PAGE_SIZE) {
      page.offset = currentOffset + size
      page.start + currentOffset
    } else {
      val newpage = LinkedPagePool.claim
      newpage.next = page
      newpage.offset = size
      page = newpage
      page.start
    }
  }
}
