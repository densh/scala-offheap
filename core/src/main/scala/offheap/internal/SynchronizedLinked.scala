package offheap
package internal

import Unsafer.unsafe

object SynchronizedLinkedPagePool {
  private var chunk: SynchronizedLinkedChunk = null
  private var page: SynchronizedLinkedPage = null
  private def allocateChunk(): Unit = {
    val start = unsafe.allocateMemory(CHUNK_SIZE)
    chunk = new SynchronizedLinkedChunk(start, chunk)
    var i = 0
    while (i < CHUNK_SIZE / PAGE_SIZE) {
      page = new SynchronizedLinkedPage(start + i * PAGE_SIZE, 0, page)
      i += 1
    }
  }
  def claim(): SynchronizedLinkedPage = this.synchronized {
    if (page == null) allocateChunk()
    val res = page
    page = res.next
    res.next = null
    res
  }
  def reclaim(head: SynchronizedLinkedPage): Unit = this.synchronized {
    var tail = head
    while (tail.next != null)  tail = tail.next
    tail.next = page
    page = head
  }
}

final class SynchronizedLinkedChunk(val start: Long, var next: SynchronizedLinkedChunk)

final class SynchronizedLinkedPage(val start: Long, var offset: Long, var next: SynchronizedLinkedPage)

final class SynchronizedLinkedRegion extends offheap.Region {
  private var page = SynchronizedLinkedPagePool.claim
  def isOpen: Boolean = page != null
  def close(): Unit = this.synchronized {
    SynchronizedLinkedPagePool.reclaim(page)
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
      val newpage = SynchronizedLinkedPagePool.claim
      newpage.next = page
      newpage.offset = size
      page = newpage
      page.start
    }
  }
}
