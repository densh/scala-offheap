package scala.offheap

/** An off-heap memory allocator. */
trait Allocator {
  def allocate(size: Size, alignment: Size = alignmentOf[Long]): Addr
  def reallocate(oldAddr: Addr, oldSize: Size, newSize: Size, alignment: Size = alignmentOf[Long]): Addr
  def free(addr: Addr): Unit
}
