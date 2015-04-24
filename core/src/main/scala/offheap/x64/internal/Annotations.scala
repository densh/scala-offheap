package offheap
package x64
package internal

import scala.annotation.StaticAnnotation
import scala.language.experimental.{macros => CanMacro}
import offheap.internal.macros

final class Layout(val fields: Fields) extends StaticAnnotation

final class Fields(val fields: (String, Class[_], Size)*)
object Fields {
  def layout[C](pairs: (String, Class[_])*): Fields = macro macros.Layout.perform[C]
}
