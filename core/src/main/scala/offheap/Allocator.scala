package offheap

/** An off-heap memory allocator. Must always implement
 *  `allocate` method. `reallocate` and `free` may not
 *  be defined for automatically managed memories like
 *  regions.
 */
trait Allocator {
  def allocate(size: Size): Addr
  def reallocate(addr: Addr, size: Size): Addr
  def free(addr: Addr): Unit
}
