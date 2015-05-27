package offheap

import offheap.internal.Memory.UNSAFE

object SystemAllocator extends Allocator {
  def allocate(size: Size): Addr               = UNSAFE.allocateMemory(size)
  def reallocate(addr: Addr, size: Size): Addr = UNSAFE.reallocateMemory(addr, size)
  def free(addr: Addr): Unit                   = UNSAFE.freeMemory(addr)
}
