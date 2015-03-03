package offheap
package internal

import offheap.internal.Setting._
import offheap.internal.Memory64._

trait Region64 extends Region {
  def memory: Memory64

  def allocate32(addr: Memory32.Size): Memory32.Addr =
    throw new UnsupportedOperationException
  def allocateRef32(addr: Memory32.Size): Ref32 =
    throw new UnsupportedOperationException

  def getRef64(addr: Memory64.Addr): Ref64
  def putRef64(addr: Memory64.Addr, ref: Ref64): Unit
}

final class PageRegion64(pool: PagePool64) extends Region64 {
  private var page = pool.claim
  val id: Long = PageRegion64.freshId.next
  val memory = pool.memory

  def isOpen = page != null

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
        0L
      }
    page.start + resOffset
  }

  def allocateRef64(size: Size): Ref64 = this.synchronized {
    Ref64(allocate64(size), this)
  }

  def getRef64(addr: Memory64.Addr): Ref64 = {
    val refAddr = memory.getLong(addr)
    if (refAddr == 0L) null
    else {
      val refRegionId = memory.getLong(addr + 8L)
      assert(refRegionId == this.id)
      Ref64(refAddr, this)
    }
  }

  def putRef64(addr: Memory64.Addr, ref: Ref64): Unit = {
    if (ref != null) {
      memory.putLong(addr, ref.addr)
      memory.putLong(addr + 8L, this.id)
    } else {
      memory.putLong(addr, 0L)
    }
  }
}
object PageRegion64 {
  private val freshId = new AtomicFreshLong
}
