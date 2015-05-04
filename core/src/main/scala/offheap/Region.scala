package offheap

import scala.language.experimental.{macros => canMacro}
import offheap.internal.macros

final class Region(private[this] val pool: Pool) extends Allocator {
  private[this] val tail = pool.claim
  tail.offset = 0
  private[this] var page = tail
  val memory = pool.memory

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

  override def close(): Unit = this.synchronized {
    checkOpen
    pool.reclaim(page, tail)
    page = null
    super.close
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
}
object Region {
  def open(implicit pool: Pool) = new Region(pool)
  def apply[T](f: Region => T)(implicit pool: Pool): T = macro macros.Region.apply
}
