package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Layout(val c: blackbox.Context) extends Common {
  import c.universe.{weakTypeOf => wt, _}

  def unwrap(pairs: Seq[Tree]): Seq[Field] =
    try pairs.map {
      case q"(${name: String}, ${Literal(Constant(tpe: Type))})" =>
        Field(name, tpe, -1)
    } catch {
      case _: MatchError =>
        abort(s"expected a static sequence of pairs but got $pairs")
    }

  def wrap(fields: Seq[Field]): Tree = q"new $offheapx.Layout(..$fields)"

  def align(fields: Seq[Field]): Seq[Field] = {
    var lastoffset = 0L
    val offsetMap = fields.map { f =>
      val tpealignment = alignmentOf(f.tpe)
      val padding =
        if (lastoffset % tpealignment == 0) 0
        else tpealignment - lastoffset % tpealignment
      val offset = lastoffset + padding
      lastoffset = offset + sizeOf(f.tpe)
      (f, offset)
    }.toMap
    fields.map { f => f.copy(offset = offsetMap(f)) }
  }

  def perform[C: WeakTypeTag](pairs: Tree*) = {
    import c.internal._, decorators._
    wt[C].typeSymbol.updateAttachment(IsClass(true))
    wrap(align(unwrap(pairs)))
  }
}
