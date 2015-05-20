package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Layout(val c: blackbox.Context) extends Common {
  import c.universe.{weakTypeOf => wt, _}
  import c.internal._, decorators._

  def inLayout(tpe: Type)(f: => Tree) = {
    tpe.typeSymbol.updateAttachment(Clazz.InLayout())
    val res = f
    tpe.typeSymbol.removeAttachment[Clazz.InLayout]
    f
  }

  def field[C: WeakTypeTag](after: Tree, tag: Tree, annots: Tree) = inLayout(wt[C]) {
    val q"${tpe: Type}" = tag
    val isData = annots.collect { case q"new $c" if c.symbol == EmbedClass => c }.nonEmpty
    val alignment =
      if (isData) {
        assertEmbeddable(tpe)
        assertNotInLayout(tpe.typeSymbol, "illegal recursive embedding")
        alignmentOfData(tpe)
      } else alignmentOf(tpe)
    val baseoffset = after match {
      case q"" => 0
      case _   =>
        val q"${prev: Field}" = ExtractField.unapply(c.typecheck(after).symbol).get.head
        prev.offset + prev.size
    }
    val padding =
      if (baseoffset % alignment == 0) 0
      else alignment - baseoffset % alignment
    q"${baseoffset + padding}"
  }

  def markComplete[C: WeakTypeTag] = {
    wt[C].typeSymbol.updateAttachment(Clazz.LayoutComplete())
    q"()"
  }
}
