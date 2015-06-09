package scala.offheap

/** An off-heap memory allocator. Must always implement
 *  `allocate` method. `reallocate` and `free` may not
 *  throw `UnsupportedOperationException` for automatically
 *  managed allocators like regions.
 */
trait Allocator {
  def allocate(size: Size): Addr
  def reallocate(addr: Addr, size: Size): Addr
  def free(addr: Addr): Unit
}
