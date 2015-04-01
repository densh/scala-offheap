package offheap
package x64

import scala.annotation.StaticAnnotation
import scala.language.experimental.{macros => CanMacro}

final case class Layout(fields: (String, Class[_], Size)*) extends StaticAnnotation
object Layout {
  def unaligned(pairs: (String, Class[_])*): Layout = macro internal.macros.Layout.unaligned
  def aligned  (pairs: (String, Class[_])*): Layout = macro internal.macros.Layout.aligned
  def packed   (pairs: (String, Class[_])*): Layout = macro internal.macros.Layout.packed
}
