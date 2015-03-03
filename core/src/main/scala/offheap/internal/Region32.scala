package offheap
package internal

import offheap.internal.Setting._
import offheap.internal.Memory32._

trait Region32 extends Region {
  def memory: Memory32

  def allocate64(addr: Memory64.Size): Memory64.Addr =
    throw new UnsupportedOperationException
  def allocateRef64(addr: Memory64.Size): Ref64 =
    throw new UnsupportedOperationException

  def getRef32(addr: Memory32.Addr): Ref32
  def putRef32(addr: Memory32.Addr, ref: Ref32): Unit
}

final class PageRegion32(pool: PagePool32) extends Region32 {
  private var page = pool.claim
  val id: Int = PageRegion32.freshId.next
  val memory = pool.memory

  def isOpen = page != null

  def close(): Unit = this.synchronized {
    assert(isOpen, "can't close region which is already closed")
    pool.reclaim(page)
    page = null
  }

  def allocate32(size: Size): Addr = this.synchronized {
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

  def allocateRef32(size: Size): Ref32 = this.synchronized {
    Ref32(allocate32(size), this)
  }

  def getRef32(addr: Memory32.Addr): Ref32 = {
    val refAddr = memory.getInt(addr)
    if (refAddr == 0) null
    else {
      val refRegionId = memory.getInt(addr + 8)
      assert(refRegionId == this.id)
      Ref32(refAddr, this)
    }
  }

  def putRef32(addr: Memory32.Addr, ref: Ref32): Unit = {
    if (ref != null) {
      memory.putInt(addr, ref.addr)
      memory.putInt(addr + 8, this.id)
    } else {
      memory.putInt(addr, 0)
    }
  }
}
object PageRegion32 {
  private val freshId = new AtomicFreshInt
}
