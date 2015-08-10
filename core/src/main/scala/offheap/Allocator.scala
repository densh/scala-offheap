package scala.offheap

/** An off-heap memory allocator. */
trait Allocator {
  def allocate(size: Size): Addr
  def reallocate(oldAddr: Addr, oldSize: Size, newSize: Size): Addr
  def free(addr: Addr): Unit
}
