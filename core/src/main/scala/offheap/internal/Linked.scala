package offheap
package internal

import Unsafe.unsafe

class LinkedPagePool {
  private var chunk: LinkedChunk = null
  private var page: LinkedPage = null
  private def compareAndSwapChunk(expected: LinkedChunk, value: LinkedChunk) =
    unsafe.compareAndSwapObject(this, LinkedPagePool.chunkFieldOffset, expected, value)
  private def compareAndSwapPage(expected: LinkedPage, value: LinkedPage) =
    unsafe.compareAndSwapObject(this, LinkedPagePool.pageFieldOffset, expected, value)
  private def allocateChunk(): Unit = {
    val start = unsafe.allocateMemory(CHUNK_SIZE)
    chunk = new LinkedChunk(start, chunk)
    val tail = new LinkedPage(start, 0, null)
    var head = tail
    var i = 1
    while (i < CHUNK_SIZE / PAGE_SIZE) {
      head = new LinkedPage(start + i * PAGE_SIZE, 0, head)
      i += 1
    }
    var commit = false
    do {
      val page = this.page
      tail.next = page
      commit = this.compareAndSwapPage(page, head)
    } while (!commit)
  }
  def claim(): LinkedPage = {
    var res: LinkedPage = null
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
  def reclaim(head: LinkedPage): Unit = {
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
object LinkedPagePool extends LinkedPagePool {
  private val chunkFieldOffset =
    unsafe.fieldOffset(classOf[LinkedPagePool].getDeclaredField("chunk"))
  private val pageFieldOffset =
    unsafe.fieldOffset(classOf[LinkedPagePool].getDeclaredField("page"))
}

final class LinkedChunk(val start: Long, var next: LinkedChunk)
object LinkedChunk {
  val nextFieldOffset = unsafe.fieldOffset(classOf[LinkedChunk].getDeclaredField("next"))
}

final class LinkedPage(val start: Long, var offset: Long, var next: LinkedPage) {
  def compareAndSwapOffset(expected: Long, value: Long) =
    unsafe.compareAndSwapLong(this, LinkedPage.offsetFieldOffset, expected, value)
  def compareAndSwapNext(expected: LinkedPage, value: LinkedPage) =
    unsafe.compareAndSwapObject(this, LinkedPage.nextFieldOffset, expected, value)
}
object LinkedPage {
  val offsetFieldOffset =
    unsafe.fieldOffset(classOf[LinkedPage].getDeclaredField("offset"))
  val nextFieldOffset =
    unsafe.fieldOffset(classOf[LinkedPage].getDeclaredField("next"))
}

final class LinkedRegion extends offheap.Region {
  private var page = LinkedPagePool.claim
  private def compareAndSwapPage(expected: LinkedPage, value: LinkedPage) =
    unsafe.compareAndSwapObject(this, LinkedRegion.pageFieldOffset, expected, value)
  protected[internal] def isOpen: Boolean = page != null
  protected[internal] def close(): Unit = {
    var commit = false
    do {
      val page = this.page
      if (page == null) throw InaccessibleRegionException
      commit = this.compareAndSwapPage(page, null)
      if (commit) LinkedPagePool.reclaim(page)
    } while (!commit)
  }
  protected[internal] def allocate(size: Size): Addr = {
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
        val newpage = LinkedPagePool.claim
        newpage.next = page
        newpage.offset = size
        if (this.compareAndSwapPage(page, newpage)) {
          res = newpage.start
        } else {
          newpage.next = null
          LinkedPagePool.reclaim(newpage)
        }
      }
    } while (res == 0L)
    res
  }
}
object LinkedRegion {
  val pageFieldOffset = unsafe.fieldOffset(classOf[LinkedRegion].getDeclaredField("page"))
}
