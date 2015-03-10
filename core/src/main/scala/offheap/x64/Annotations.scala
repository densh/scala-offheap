package offheap
package x64

import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation

final class data extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro internal.macros.Annotations.data
}

final class enum extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro internal.macros.Annotations.enum
}
