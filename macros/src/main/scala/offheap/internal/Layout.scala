package offheap
package internal

import scala.language.experimental.{macros => CanMacro}

object Layout {
  def field[C](after: Any, tag: Class[_], annots: Any): Long =
    macro macros.Layout.field[C]
  def markComplete[C]: Unit =
    macro macros.Layout.markComplete[C]
}
