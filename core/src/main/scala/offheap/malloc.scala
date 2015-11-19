package scala.offheap

import scala.offheap.internal.pad
import scala.offheap.internal.SunMisc.UNSAFE

/** Underyling OS allocator that does not attempt
 *  to perform any automatic memory management
 *  (all allocations must have accompanied calls to free.)
 */
object malloc extends Allocator {
  def allocate(size: Size, alignment: Size): Addr =
    if (alignment <= alignmentOf[Long])
      UNSAFE.allocateMemory(size)
    else
      pad(UNSAFE.allocateMemory(size + alignment), alignment)

  def reallocate(oldAddr: Addr, oldSize: Size, newSize: Size, alignment: Size = alignmentOf[Long]): Addr =
    if (alignment <= alignmentOf[Long])
      UNSAFE.reallocateMemory(oldAddr, newSize)
    else
      pad(UNSAFE.reallocateMemory(oldAddr, newSize + alignment), alignment)

  def free(addr: Addr): Unit =
    UNSAFE.freeMemory(addr)
}
