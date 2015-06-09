package offheap

import offheap.internal.JemallocWrapper

/** jemalloc based allocator that does not attempt
  * to perform any automatic memory management
  * (all allocations must have accompanied calls to free.)
  */
object jemalloc extends Allocator {
  def allocate(size: Size): Addr = {
    val address = JemallocWrapper.malloc(size)

    if (address <= 0) {
      throw new IllegalArgumentException(s"Failed to allocate $size bytes")
    }

    address
  }

  def reallocate(addr: Addr, size: Size): Addr = {
    val newAddress = JemallocWrapper.realloc(addr, size)

    if (newAddress <= 0) {
      throw new IllegalArgumentException(s"Failed to reallocate memory at address $addr to new size $size")
    }

    newAddress
  }

  def free(addr: Addr): Unit = JemallocWrapper.free(addr)
}
