package offheap

import internal.Sanitizer
import internal.Checked

trait Allocator {
  def allocate(size: Size): Addr
  private[this] val id: Long =
    if (Checked.MEMORY) Sanitizer.register()
    else 0L
  def close: Unit =
    if (Checked.MEMORY) Sanitizer.unregister(id)
  protected def wrap(addr: Addr): Addr = {
    if (Checked.MEMORY) Sanitizer.pack(this.id, addr)
    else addr
  }
}
object Allocator {
  def apply() = new UnsafeAllocator()
}
