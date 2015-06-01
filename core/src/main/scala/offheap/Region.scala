package offheap

import scala.language.experimental.{macros => canMacro}
import offheap.internal.macros
import offheap.internal.Sanitizer
import offheap.internal.Checked

/** Scoped region-based allocator. Supports allocations up to
 *  the size of a page in memory pool. Memory is reclaimed back
 *  to the pool in constant-time once the region is closed.
 */
final class Region(private[this] val pool: Pool) extends Allocator {
  private[this] val tail = pool.claim
  tail.offset = 0
  private[this] var page = tail
  private[this] val id: Long =
    if (Checked.MEMORY) Sanitizer.register()
    else 0L

  def isOpen   = page != null
  def isClosed = page == null

  private def checkOpen(): Unit =
    if (page == null)
      throw new IllegalArgumentException(s"$this has already been closed")

  private def pad(addr: Addr) = {
    val alignment = sizeOf[Long]
    val padding =
      if (addr % alignment == 0) 0
      else alignment - addr % alignment
    addr + padding
  }

  private def wrap(addr: Addr): Addr = {
    if (Checked.MEMORY) Sanitizer.pack(this.id, addr)
    else addr
  }

  def close(): Unit = this.synchronized {
    checkOpen
    pool.reclaim(page, tail)
    page = null
    if (Checked.MEMORY) Sanitizer.unregister(id)
  }

  def allocate(size: Size): Addr = this.synchronized {
    checkOpen
    if (size > pool.pageSize)
      throw new IllegalArgumentException("can't allocate object larger than the virtual page")
    val currentOffset = page.offset
    val paddedOffset = pad(currentOffset)
    val resOffset =
      if (paddedOffset + size <= pool.pageSize) {
        page.offset = paddedOffset + size
        paddedOffset
      } else {
        val newpage = pool.claim
        newpage.next = page
        newpage.offset = size
        page = newpage
        0L
      }
    wrap(page.start + resOffset)
  }

  def reallocate(addr: Addr, size: Size): Addr =
    throw new UnsupportedOperationException

  def free(addr: Addr): Unit =
    throw new UnsupportedOperationException
}
object Region {
  def open(implicit pool: Pool) = new Region(pool)
  def apply[T](f: Region => T)(implicit pool: Pool): T = macro macros.Region.apply
}
