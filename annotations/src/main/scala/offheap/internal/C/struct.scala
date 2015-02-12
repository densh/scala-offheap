package offheap
package internal
package C

import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation

final class struct extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro internal.macros.Annotations.struct
}
