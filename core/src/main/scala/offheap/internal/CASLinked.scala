package offheap
package internal

import Unsafer.unsafe

object CASLinkedPagePool {
  @volatile private var chunk: CASLinkedChunk = null
  @volatile private var page: CASLinkedPage = null
  private def sliceChunk(chunk: CASLinkedChunk): Unit = {
    val start = chunk.start
    val tail = new CASLinkedPage(start, 0, null)
    var head = tail
    var i = 1
    while (i < CHUNK_SIZE / PAGE_SIZE) {
      head = new CASLinkedPage(start + i * PAGE_SIZE, 0, head)
      i += 1
    }
    var commit = false
    do {
      val page = this.page
      tail.next = page
      commit = unsafe.compareAndSwapObject(this, Offset.PoolPage, page, head)
    } while (!commit)
  }
  private def allocateChunk(): Unit = {
    val newChunk = new CASLinkedChunk(unsafe.allocateMemory(CHUNK_SIZE), null)
    var commit = false
    do {
      val chunk = this.chunk
      newChunk.next = chunk
      commit = unsafe.compareAndSwapObject(this, Offset.PoolChunk, chunk, newChunk)
    } while(!commit)
    sliceChunk(newChunk)
  }
  def claim(): CASLinkedPage = {
    var res: CASLinkedPage = null
    do {
      val page = this.page
      if (page == null)
        allocateChunk()
      else if (unsafe.compareAndSwapObject(this, Offset.PoolPage, page, page.next)) {
        page.next = null
        res = page
      }
    } while (res == null)
    res
  }
  def reclaim(head: CASLinkedPage): Unit = {
    var tail = head
    while (tail.next != null)  tail = tail.next
    var commit = false
    do {
      val page = this.page
      tail.next = page
      commit = unsafe.compareAndSwapObject(this, Offset.PoolPage, page, head)
    } while(!commit)
  }
}

final class CASLinkedChunk(val start: Long, var next: CASLinkedChunk)

final class CASLinkedPage(val start: Long, @volatile var offset: Long, var next: CASLinkedPage)

final class CASLinkedRegion extends offheap.Region {
  @volatile private var page = CASLinkedPagePool.claim
  def isOpen: Boolean = page != null
  def close(): Unit = {
    var commit = false
    do {
      val page = this.page
      if (page == null) throw InaccessibleRegionException
      commit = unsafe.compareAndSwapObject(this, Offset.RegionPage, page, null)
      if (commit) CASLinkedPagePool.reclaim(page)
    } while (!commit)
  }
  private def bailout(newpage: CASLinkedPage): Unit = {
    newpage.next = null
    CASLinkedPagePool.reclaim(newpage)
  }
  def allocate(size: Size): Addr = {
    if (size > PAGE_SIZE) throw new IllegalArgumentException
    var commit = false
    var res = 0L
    do {
      val page = this.page
      if (page == null) throw InaccessibleRegionException
      val pageOffset = page.offset
      if (pageOffset + size <= PAGE_SIZE) {
        val newOffset = pageOffset + size
        commit = unsafe.compareAndSwapLong(page, Offset.PageOffset, pageOffset, newOffset)
        res = page.start + pageOffset
      } else {
        val newpage = CASLinkedPagePool.claim
        newpage.next = page
        newpage.offset = size
        commit = unsafe.compareAndSwapObject(this, Offset.RegionPage, page, newpage)
        res = newpage.start
        if (!commit) bailout(newpage)
      }
    } while (!commit)
    res
  }
}
