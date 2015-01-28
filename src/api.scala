package regions

import scala.language.dynamics
import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation

final class Region private[regions](
  private[regions] val region: internal.rt.Region
) extends AnyVal

object Region {
  def apply[T](f: Region => T): T = macro internal.macros.Region.alloc[T]
}

/** Marker trait for offheap classes */
trait Ref extends Any

/** Exception that is thrown whenever null is dereferenced. */
case object NullRefException extends Exception

/** */
case object InaccessiblePageException extends  Exception

final class offheap extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro internal.macros.Annotations.offheap
}
