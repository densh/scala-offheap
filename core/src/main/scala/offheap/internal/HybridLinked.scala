package offheap
package internal

import Unsafe.unsafe

class HybridLinkedPagePool {
  private var chunk: HybridLinkedChunk = null
  private var page: HybridLinkedPage = null
  private def compareAndSwapChunk(expected: HybridLinkedChunk, value: HybridLinkedChunk) =
    unsafe.compareAndSwapObject(this, HybridLinkedPagePool.chunkFieldOffset, expected, value)
  private def compareAndSwapPage(expected: HybridLinkedPage, value: HybridLinkedPage) =
    unsafe.compareAndSwapObject(this, HybridLinkedPagePool.pageFieldOffset, expected, value)
  private def allocateChunk(): Unit = {
    val start = unsafe.allocateMemory(CHUNK_SIZE)
    chunk = new HybridLinkedChunk(start, chunk)
    val tail = new HybridLinkedPage(start, 0, null)
    var head = tail
    var i = 1
    while (i < CHUNK_SIZE / PAGE_SIZE) {
      head = new HybridLinkedPage(start + i * PAGE_SIZE, 0, head)
      i += 1
    }
    var commit = false
    do {
      val page = this.page
      tail.next = page
      commit = this.compareAndSwapPage(page, head)
    } while (!commit)
  }
  def claim(): HybridLinkedPage = {
    var res: HybridLinkedPage = null
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
  def reclaim(head: HybridLinkedPage): Unit = {
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
object HybridLinkedPagePool extends HybridLinkedPagePool {
  private val chunkFieldOffset =
    unsafe.fieldOffset(classOf[HybridLinkedPagePool].getDeclaredField("chunk"))
  private val pageFieldOffset =
    unsafe.fieldOffset(classOf[HybridLinkedPagePool].getDeclaredField("page"))
}

final class HybridLinkedChunk(val start: Long, var next: HybridLinkedChunk)

final class HybridLinkedPage(val start: Long, var offset: Long, var next: HybridLinkedPage)

final class HybridLinkedRegion extends offheap.Region {
  private var page = HybridLinkedPagePool.claim
  def isOpen: Boolean = page != null
  def close(): Unit = this.synchronized {
    HybridLinkedPagePool.reclaim(page)
    page = null
  }
  def allocate(size: Size): Addr = this.synchronized {
    assert(isOpen)
    assert(size <= PAGE_SIZE)
    val currentOffset = page.offset
    val resOffset =
      if (currentOffset + size <= PAGE_SIZE) {
        page.offset = (currentOffset + size).toShort
        currentOffset
      } else {
        val newpage = HybridLinkedPagePool.claim
        newpage.next = page
        newpage.offset = size.toShort
        page = newpage
        0
      }
    page.start + resOffset
  }
}
