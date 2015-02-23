package offheap
package internal

import Unsafer.unsafe

class CASLinkedPagePool {
  @volatile private var chunk: CASLinkedChunk = null
  @volatile private var page: CASLinkedPage = null
  private def compareAndSwapChunk(expected: CASLinkedChunk, value: CASLinkedChunk) =
    unsafe.compareAndSwapObject(this, CASLinkedPagePool.chunkFieldOffset, expected, value)
  private def compareAndSwapPage(expected: CASLinkedPage, value: CASLinkedPage) =
    unsafe.compareAndSwapObject(this, CASLinkedPagePool.pageFieldOffset, expected, value)
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
      commit = this.compareAndSwapPage(page, head)
    } while (!commit)
  }
  private def allocateChunk(): Unit = {
    val newChunk = new CASLinkedChunk(unsafe.allocateMemory(CHUNK_SIZE), null)
    var commit = false
    do {
      val chunk = this.chunk
      newChunk.next = chunk
      commit = this.compareAndSwapChunk(chunk, newChunk)
    } while(!commit)
    sliceChunk(chunk)
  }
  def claim(): CASLinkedPage = {
    var res: CASLinkedPage = null
    do {
      val page = this.page
      if (page == null)
        allocateChunk()
      else if (this.compareAndSwapPage(page, page.next)) {
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
      commit = this.compareAndSwapPage(page, head)
    } while(!commit)
  }
}
object CASLinkedPagePool extends CASLinkedPagePool {
  private val chunkFieldOffset =
    unsafe.fieldOffset(classOf[CASLinkedPagePool].getDeclaredField("chunk"))
  private val pageFieldOffset =
    unsafe.fieldOffset(classOf[CASLinkedPagePool].getDeclaredField("page"))
}

final class CASLinkedChunk(val start: Long, var next: CASLinkedChunk)

final class CASLinkedPage(val start: Long, @volatile var offset: Long, var next: CASLinkedPage) {
  def compareAndSwapOffset(expected: Long, value: Long) =
    unsafe.compareAndSwapLong(this, CASLinkedPage.offsetFieldOffset, expected, value)
}
object CASLinkedPage {
  val offsetFieldOffset =
    unsafe.fieldOffset(classOf[CASLinkedPage].getDeclaredField("offset"))
}

final class CASLinkedRegion extends offheap.Region {
  @volatile private var page = CASLinkedPagePool.claim
  private def compareAndSwapPage(expected: CASLinkedPage, value: CASLinkedPage) =
    unsafe.compareAndSwapObject(this, CASLinkedRegion.pageFieldOffset, expected, value)
  def isOpen: Boolean = page != null
  def close(): Unit = {
    var commit = false
    do {
      val page = this.page
      if (page == null) throw InaccessibleRegionException
      commit = this.compareAndSwapPage(page, null)
      if (commit) CASLinkedPagePool.reclaim(page)
    } while (!commit)
  }
  def allocate(size: Size): Addr = {
    if (size > PAGE_SIZE) throw new IllegalArgumentException
    var res = 0L
    do {
      val page = this.page
      if (page == null) throw InaccessibleRegionException
      val pageOffset = page.offset
      if (pageOffset + size <= PAGE_SIZE) {
        val newOffset = pageOffset + size
        if (page.compareAndSwapOffset(pageOffset, newOffset)) {
          res = page.start + pageOffset
        }
      } else {
        val newpage = CASLinkedPagePool.claim
        newpage.next = page
        newpage.offset = size
        if (this.compareAndSwapPage(page, newpage)) {
          res = newpage.start
        } else {
          newpage.next = null
          CASLinkedPagePool.reclaim(newpage)
        }
      }
    } while (res == 0L)
    res
  }
}
object CASLinkedRegion {
  private val pageFieldOffset = unsafe.fieldOffset(classOf[CASLinkedRegion].getDeclaredField("page"))
}
