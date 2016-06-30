package scala.offheap

import scala.language.experimental.{macros => canMacro}
import scala.offheap.internal.macros

/** Family of scoped memory allocators. Allocated memory
 *  is available as long as execution is still in given
 *  region scope and is cleaned up once it's done
 *
 *  A few memory management implementations are available.
 *  It's possible to pick the desirable implementation using
 *  an implicit instance of `Region.Props`.
 */
trait Region extends Allocator {
  protected def checkOpen(): Unit =
    if (!isOpen)
      throw new RegionClosedException(this)
  def isOpen: Boolean
  def close(): Unit = checkOpen
  def reallocate(oldAddr: Addr, oldSize: Size, newSize: Size, alignment: Size): Addr = {
    checkOpen
    if (newSize <= oldSize)
      oldAddr
    else {
      val newAddr = allocate(newSize, alignment)
      Memory.copy(oldAddr, newAddr, oldSize)
      newAddr
    }
  }
  def free(addr: Addr): Unit =
    checkOpen
}
object Region {
  /** Object that contains the configuration information necessary to
   *  open a region. Used as a way to implicitly define which
   *  region implementation strategies to pick in given scope.
   */
  trait Props { def open(): Region }
  object Props {
    def apply(pool: Pool) = PoolRegion.Props(pool)
  }

  def open(implicit props: Props): Region = props.open
  def apply[T](f: Region => T)(implicit props: Props): T = macro macros.Region.apply
}

