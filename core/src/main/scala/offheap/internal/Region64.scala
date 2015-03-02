package offheap
package internal

import offheap.internal.Setting._
import offheap.internal.Memory64._

final class PageRegion64(pool: PagePool64) extends Region {
  private var page = pool.claim
  def isOpen: Boolean = page != null
  def close(): Unit = this.synchronized {
    assert(isOpen, "can't close region which is already closed")
    pool.reclaim(page)
    page = null
  }
  def allocate64(size: Size): Addr = this.synchronized {
    assert(isOpen, "can't allocate in closed region")
    assert(size <= pageSize, "can't allocate object larger than the virtual page")
    val currentOffset = page.offset
    val resOffset =
      if (currentOffset + size <= pageSize) {
        page.offset = (currentOffset + size).toShort
        currentOffset
      } else {
        val newpage = pool.claim
        newpage.next = page
        newpage.offset = size.toShort
        page = newpage
        0
      }
    page.start + resOffset
  }
}
