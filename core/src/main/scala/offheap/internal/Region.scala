package offheap
package internal

import C._
import Unsafe.unsafe
import scala.language.experimental.{macros => CanMacro}

trait Region { self: offheap.Region =>
  protected[internal] def isOpen: Boolean
  protected[internal] def isClosed: Boolean
  protected[internal] def close(): Unit
  protected[internal] def allocate(size: Size): Addr
}
object Region {
  def open(): offheap.Region = new UncheckedRegion
  def close(r: offheap.Region): Unit = r.close()
  def isOpen(r: offheap.Region): Boolean = r.isOpen
  def isClosed(r: offheap.Region): Boolean = r.isClosed
  def allocate(r: offheap.Region, size: Size): Addr = r.allocate(size)
}

final class UncheckedRegion extends offheap.Region {
  private var pool   = PagePool.currentPool.get
  private var dirty  = AddrStack.alloc(8)
  private var page   = pool.claim
  private var offset = 0

  protected[internal] def isOpen = pool != null

  protected[internal] def isClosed = pool == null

  protected[internal] def close(): Unit = {
    assert(isOpen)
    pool.reclaim(page)
    pool.reclaimStack(dirty)
    AddrStack.free(dirty)
    pool = null
  }

  protected[internal] def allocate(size: Size): Addr = {
    assert(isOpen)
    assert(size < PAGE_SIZE)
    val currentOffset = offset
    val resOffset =
      if (currentOffset + size < PAGE_SIZE) {
        offset = (currentOffset + size).toShort
        currentOffset
      } else {
        AddrStack.push(dirty, page)
        page   = pool.claim
        offset = size.toShort
        0
      }
    page + resOffset
  }
}
