package offheap
package internal

import Unsafe.unsafe

object Region {
  import PagePool.currentPool

  final class Handle(var page: Addr, var offset: Short, val dirty: AddrStack.T)

  def open(): Region =
    new Region(new Handle(currentPool.get.claim, 0, AddrStack.alloc(8)))

  def close(r: Region): Unit = {
    val h    = r.handle.asInstanceOf[Handle]
    val pool = currentPool.get
    pool.reclaim(h.page)
    pool.reclaimStack(h.dirty)
    AddrStack.free(h.dirty)
  }

  def allocate(r: Region, size: Size): Addr = {
    assert(size < PAGE_SIZE)
    val pool   = currentPool.get
    val h      = r.handle.asInstanceOf[Handle]
    val offset = h.offset
    val resOffset =
      if (offset + size < PAGE_SIZE) {
        h.offset = (offset + size).toShort
        offset
      } else {
        AddrStack.push(h.dirty, h.page)
        h.page   = pool.claim
        h.offset = size.toShort
        0
      }
    h.page + resOffset
  }
}




