package scala.offheap

import scala.offheap.internal.JemallocWrapper

/** jemalloc based allocator that does not attempt
  * to perform any automatic memory management
  * (all allocations must have accompanied calls to free.)
  */
object jemalloc extends Allocator {
  def allocate(size: Size): Addr = {
    val address = JemallocWrapper.malloc(validSize(size))

    if (address <= 0) {
      throw new OutOfMemoryError(s"Failed to allocate $size bytes")
    }

    address
  }

  def reallocate(addr: Addr, size: Size): Addr = {
    val newAddress = JemallocWrapper.realloc(addr, validSize(size))

    if (newAddress <= 0) {
      throw new OutOfMemoryError(s"Failed to reallocate memory at address $addr to new size $size")
    }

    newAddress
  }

  def free(addr: Addr): Unit = JemallocWrapper.free(addr)

  private def validSize(size: Size): Size =
    if (size < 0) {
      throw new IllegalArgumentException(s"allocate/reallocate sizes must be positive ($size) provided")
    } else {
      size
    }
}
