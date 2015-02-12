package offheap

import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation

final class offheap extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro internal.macros.Annotations.offheap
}
