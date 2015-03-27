package offheap

import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation

final class unsafe extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro internal.macros.Unsafe.annotation
}
object unsafe {
  def apply[T](f: => T): T =
    macro internal.macros.Unsafe.scope
}
