package offheap

import internal.Sanitizer
import internal.CheckedHolder.CHECKED

trait Allocator {
  def allocate(size: Size): Addr
  private[this] val id: Long =
    if (CHECKED) Sanitizer.register()
    else 0L
  def close: Unit =
    if (CHECKED) Sanitizer.unregister(id)
  protected def wrap(addr: Addr): Addr = {
    if (CHECKED) Sanitizer.pack(this.id, addr)
    else addr
  }
}
object Allocator {
  def apply() = new UnsafeAllocator()
}
