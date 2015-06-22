package scala.offheap

import scala.offheap.internal.JemallocWrapper

/** jemalloc based allocator that does not attempt
  * to perform any automatic memory management
  * (all allocations must have accompanied calls to free.)
  */
object jemalloc extends Allocator {
  def allocate(size: Size): Addr = {
    val address = JemallocWrapper.malloc(validSize(size))

    address match {
      case -1 => throw new IllegalArgumentException(s"Invalid allocation size $size passed for platform architecture");
      case -2 => throw new OutOfMemoryError(s"Failed to allocate $size bytes")
      case _  => address
    }
  }

  def reallocate(addr: Addr, size: Size): Addr = {
    val newAddress = JemallocWrapper.realloc(addr, validSize(size))

    newAddress match {
      case -1 => throw new IllegalArgumentException(s"Invalid reallocation size $size passed for platform architecture");
      case -2 => throw new OutOfMemoryError(s"Failed to reallocate memory at address $addr to new size $size")
      case _  => newAddress
    }
  }

  def free(addr: Addr): Unit = JemallocWrapper.free(addr)

  private def validSize(size: Size): Size =
    if (size < 0)
      throw new IllegalArgumentException(s"allocate/reallocate sizes must be positive ($size) provided")
    else if (JemallocWrapper.Is32BitWordSize && size > JemallocWrapper.Max32BitAllocationRequestSize)
      throw new IllegalArgumentException(s"allocate/reallocate sizes must be less than ${JemallocWrapper.Max32BitAllocationRequestSize } for 32 bit architectures")
    else size
}
