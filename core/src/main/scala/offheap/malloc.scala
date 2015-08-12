package scala.offheap

import scala.offheap.internal.SunMisc.UNSAFE

/** Underyling OS allocator that does not attempt
 *  to perform any automatic memory management
 *  (all allocations must have accompanied calls to free.)
 */
object malloc extends Allocator {
  def allocate(size: Size): Addr =
    UNSAFE.allocateMemory(size)
  def reallocate(oldAddr: Addr, oldSize: Size, newSize: Size): Addr =
    UNSAFE.reallocateMemory(oldAddr, newSize)
  def free(addr: Addr): Unit =
    UNSAFE.freeMemory(addr)
}
