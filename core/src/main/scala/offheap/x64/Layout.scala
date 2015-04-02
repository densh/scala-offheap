package offheap
package x64

import scala.annotation.StaticAnnotation
import scala.language.experimental.{macros => CanMacro}
import offheap.internal.macros

final class Layout(val fields: (String, Class[_], Size)*)
object Layout {
  def unaligned(pairs: (String, Class[_])*): Layout = macro macros.Layout.unaligned
  def aligned  (pairs: (String, Class[_])*): Layout = macro macros.Layout.aligned
  def packed   (pairs: (String, Class[_])*): Layout = macro macros.Layout.packed
}
