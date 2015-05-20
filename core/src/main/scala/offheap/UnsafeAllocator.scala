package offheap

import offheap.internal.Memory.UNSAFE

class UnsafeAllocator extends Allocator {
  private class Alloc(val addr: Addr, val next: Alloc)
  private[this] var closed = false
  private[this] var alloc: Alloc = null
  protected override def finalize(): Unit =
    if (!closed) close

  override def close(): Unit = this.synchronized {
    if (closed)
      throw new IllegalArgumentException
    super.close
    while (alloc != null) {
      UNSAFE.freeMemory(alloc.addr)
      alloc = alloc.next
    }
    closed = true
  }

  def allocate(size: Size): Addr = this.synchronized {
    val addr = UNSAFE.allocateMemory(size)
    alloc = new Alloc(addr, alloc)
    wrap(addr)
  }
}
object UnsafeAllocator {
  def apply() = new UnsafeAllocator()
}
