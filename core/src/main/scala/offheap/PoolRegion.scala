package scala.offheap

import scala.offheap.internal.pad

/** An optimized implementation of region that performs all
 *  allocations sequentially in pages that are claimed from
 *  memory pool. It can not perform allocations bigger than
 *  memory pool page. This limitation is trade off to achieve
 *  superior allocation and deallocation performance.
 */
final class PoolRegion(private[this] val pool: Pool) extends Region {
  private[this] val tail = pool.claim
  tail.offset = 0
  private[this] var page = tail

  def isOpen = page != null

  override def close(): Unit = this.synchronized {
    super.close
    pool.reclaim(page, tail)
    page = null
  }

  def allocate(size: Size, alignment: Size): Addr = this.synchronized {
    checkOpen
    if (size > pool.pageSize)
      throw new IllegalArgumentException("can't allocate object larger than the virtual page")
    if (alignment > pool.pageSize)
      throw new IllegalArgumentException("can't align to size bigger than the virtual page")
    val currentOffset = page.offset
    val paddedOffset = pad(currentOffset, alignment)
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
    page.start + resOffset
  }
}
object PoolRegion {
  final case class Props(pool: Pool = Pool()) extends Region.Props {
    def open = PoolRegion.open(pool)
  }
  def open(pool: Pool): PoolRegion = new PoolRegion(pool)
}

