package scala.offheap
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

  def field[C: WeakTypeTag, T: WeakTypeTag](after: Tree, annots: Tree) = inLayout(wt[C]) {
    val tpe = wt[T]
    val isEmbed = annots.collect { case q"new $c" if c.symbol == EmbedClass => c }.nonEmpty
    val alignment =
      if (isEmbed) {
        assertEmbeddable(tpe)
        assertNotInLayout(tpe.typeSymbol, "illegal recursive embedding")
        alignmentOfEmbed(tpe)
      } else alignmentOf(tpe)
    val baseoffset = after match {
      case q"" => 0
      case _   =>
        val q"${prev: Field}" = ExtractField.unapply(c.typecheck(after).symbol).get.head
        prev.offset + prev.size
    }
    q"${padded(baseoffset, alignment)}"
  }

  def markComplete[C: WeakTypeTag] = {
    wt[C].typeSymbol.updateAttachment(Clazz.LayoutComplete())
    q"()"
  }
}
