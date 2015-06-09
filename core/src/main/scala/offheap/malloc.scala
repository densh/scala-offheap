package scala.offheap

import offheap.internal.SunMisc.UNSAFE

/** Underyling OS allocator that does not attempt
 *  to perform any automatic memory management
 *  (all allocations must have accompanied calls to free.)
 */
object malloc extends Allocator {
  def allocate(size: Size): Addr               = UNSAFE.allocateMemory(size)
  def reallocate(addr: Addr, size: Size): Addr = UNSAFE.reallocateMemory(addr, size)
  def free(addr: Addr): Unit                   = UNSAFE.freeMemory(addr)
}
