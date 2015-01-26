package regions

import scala.language.dynamics
import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation
import regions.internal.{rt, macros}

final class Region[Id <: Int] private[regions](
  private[regions] var node: rt.Node,
  private[regions] var offset: Long,
  private[regions] var closed: Boolean = false
) {
  def isOpen: Boolean = !closed
  def isClosed: Boolean = closed
  def close(): Unit = {
    if (!closed) {
      rt.disposeRegion(this)
      closed = true
    }
  }
  protected override def finalize(): Unit = {
    close()
  }
}
object Region {
  def open[Id <: Int](implicit id: FreshId[Id]) =
    rt.allocRegion().asInstanceOf[Region[Id]]
}

/** Marker trait for offheap classes */
trait Ref[R <: Region[_]] extends Any

/** Exception that is thrown whenever null is dereferenced. */
case object NullRefException extends Exception
case object InaccessibleRegionException extends Exception

final class offheap extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro macros.Annotations.offheap
}
