package scala.offheap

import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation
import offheap.internal.macros

/** Macro annotation that transforms given class into
 *  case-class-like offheap class.
 */
final class data extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro macros.Annotations.data
}

/** Macro annotation that transforms given class into
 *  sealed-abstract-class-like offheap class. All
 *  off-heap classes defined in its companion form an
 *  off-heap child-parent relationship.
 */
final class enum extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any =
    macro macros.Annotations.enum
}

/** Annotation that marks fields as embedded.
 *  Such fields are effectively inlined into the
 *  outer class in terms of their data layout.
 */
final class embed extends StaticAnnotation
