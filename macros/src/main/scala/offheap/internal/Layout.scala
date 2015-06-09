package scala.offheap
package internal

import scala.language.experimental.{macros => CanMacro}

object Layout {
  def field[C, T](after: Any, annots: Any): Long =
    macro macros.Layout.field[C, T]
  def markComplete[C]: Unit =
    macro macros.Layout.markComplete[C]
}
