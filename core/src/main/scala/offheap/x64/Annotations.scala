package offheap
package x64

import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation
import offheap.internal.macros

final class data extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro macros.Annotations.data
}

final class enum extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro macros.Annotations.enum
}
