package scala.offheap

import scala.language.experimental.{macros => canMacro}
import offheap.internal.macros
import offheap.internal.Sanitizer
import offheap.internal.Checked

/** Family of scoped memory allocators. Allocated memory
 *  is available as long as execution is still in given
 *  region scope and is cleaned up once it's done
 *
 *  A few memory management implemenations are available.
 *  It's possible to pick the desirable implementation using
 *  an implicit instance of `Region.Props`.
 */
trait Region extends Allocator {
  protected val id: Long =
    if (Checked.MEMORY) Sanitizer.register()
    else 0L
  protected def checkOpen(): Unit =
    if (!isOpen)
      throw new IllegalArgumentException(s"$this has already been closed")
  protected def wrap(addr: Addr): Addr = {
    if (Checked.MEMORY) Sanitizer.pack(this.id, addr)
    else addr
  }
  def isOpen: Boolean
  def close(): Unit = {
    checkOpen
    if (Checked.MEMORY) Sanitizer.unregister(id)
  }
  def reallocate(addr: Addr, size: Size): Addr =
    throw new UnsupportedOperationException
  def free(addr: Addr): Unit =
    throw new UnsupportedOperationException
}
object Region {
  /** Object that contains the configuration information necessary to
   *  open a region. Used as a way to implicitly define which
   *  region implementation strategies to pick in given scope.
   */
  trait Props { def open(): Region }
  object Props {
    def apply(pool: Pool = Pool()) = PoolRegion.Props(pool)
    def direct(alloc: Allocator = malloc) = DirectRegion.Props(alloc)
  }

  def open(implicit props: Props): Region = props.open
  def apply[T](f: Region => T)(implicit props: Props): T = macro macros.Region.apply
}

