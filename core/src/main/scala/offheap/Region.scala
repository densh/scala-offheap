package offheap

import scala.language.experimental.{macros => canMacro}
import offheap.internal.macros
import offheap.internal.Sanitizer
import offheap.internal.Checked

/** Scoped region-based allocator. Supports allocations up to
 *  the size of a page in memory pool. Memory is reclaimed back
 *  to the pool in constant-time once the region is closed.
 */
trait Region extends Allocator {
  protected val id: Long =
    if (Checked.MEMORY) Sanitizer.register()
    else 0L
  protected def checkOpen(): Unit =
    if (!isOpen)
      throw new IllegalArgumentException(s"$this has already been closed")
  protected def wrap(addr: Addr): Addr = {
    if (Checked.MEMORY) Sanitizer.pack(this.id, addr)
    else addr
  }
  def isOpen: Boolean
  def close(): Unit = {
    checkOpen
    if (Checked.MEMORY) Sanitizer.unregister(id)
  }
  def reallocate(addr: Addr, size: Size): Addr =
    throw new UnsupportedOperationException
  def free(addr: Addr): Unit =
    throw new UnsupportedOperationException
}
object Region {
  trait Policy { def open: Region }
  def open(implicit policy: Policy): Region = policy.open
  def apply[T](f: Region => T)(implicit policy: Policy): T = macro macros.Region.apply
}

final class PoolRegion(private[this] val pool: Pool) extends Region {
  private[this] val tail = pool.claim
  tail.offset = 0
  private[this] var page = tail

  private def pad(addr: Addr) = {
    val alignment = sizeOf[Long]
    val padding =
      if (addr % alignment == 0) 0
      else alignment - addr % alignment
    addr + padding
  }

  def isOpen = page != null

  override def close(): Unit = this.synchronized {
    super.close
    pool.reclaim(page, tail)
    page = null
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
object PoolRegion {
  final case class Policy(pool: Pool = Pool()) extends Region.Policy {
    def open = PoolRegion.open(pool)
  }
  def open(pool: Pool): PoolRegion = new PoolRegion(pool)
}

final class DirectRegion private(private[this] var alloc: Allocator) extends Region {
  if (alloc == null)
    throw new IllegalArgumentException("allocator must not be null")

  private final class Allocation(val addr: Addr, val next: Allocation)
  private[this] var allocation: Allocation = null
  def isOpen: Boolean =
    alloc != null
  override def close(): Unit = this.synchronized {
    super.close
    while (allocation != null) {
      try {
        alloc.free(allocation.addr)
        allocation = allocation.next
      } catch {
        case _: UnsupportedOperationException =>
          allocation = null
      }
    }
    alloc = null
  }
  def allocate(size: Size) = this.synchronized {
    checkOpen
    val addr = alloc.allocate(size)
    allocation = new Allocation(addr, allocation)
    wrap(addr)
  }
}
object DirectRegion {
  final case class Policy(alloc: Allocator = malloc) extends Region.Policy {
    def open = DirectRegion.open(alloc)
  }
  def open(alloc: Allocator): DirectRegion = new DirectRegion(alloc)
}
