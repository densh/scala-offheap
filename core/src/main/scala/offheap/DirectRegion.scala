package scala.offheap

/** An implementation of region that delegates all allocations
 *  to an underlying allocator, keeps the record of them, and
 *  frees them up when region is closed.
 */
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
  final case class Props(alloc: Allocator = malloc) extends Region.Props {
    def open = DirectRegion.open(alloc)
  }
  def open(alloc: Allocator): DirectRegion = new DirectRegion(alloc)
}
