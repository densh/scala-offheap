package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Layout(val c: blackbox.Context) extends Common {
  import c.universe.{weakTypeOf => wt, _}
  import c.internal._, decorators._

  def fieldOffset(after: Tree, tag: Tree, annots: Tree) = {
    val q"${tpe: Type}" = tag
    val baseoffset = after match {
      case q"" => 0
      case _   =>
        val q"${prev: Field}" = ExtractField.unapply(c.typecheck(after).symbol).get.head
        prev.offset + prev.size
    }
    val isData: Boolean = false
    val alignment = if (isData) alignmentOfData(tpe) else alignmentOf(tpe)
    val padding =
      if (baseoffset % alignment == 0) 0
      else alignment - baseoffset % alignment
    q"${baseoffset + padding}"
  }
}
