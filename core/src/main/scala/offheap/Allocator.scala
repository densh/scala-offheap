package offheap

trait Allocator {
  def allocate(size: Size): Addr
  def reallocate(addr: Addr, size: Size): Addr
  def free(addr: Addr): Unit
}
