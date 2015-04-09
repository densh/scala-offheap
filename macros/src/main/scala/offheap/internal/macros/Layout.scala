package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Layout(val c: blackbox.Context) extends Common {
  import c.universe._

  def unwrap(pairs: Seq[Tree]): Seq[Field] =
    try pairs.map {
      case q"(${name: String}, ${Literal(Constant(tpe: Type))})" =>
        Field(name, tpe, -1)
    } catch {
      case _: MatchError =>
        abort(s"expected a static sequence of pairs but got $pairs")
    }

  def wrap(fields: Seq[Field]): Tree = q"new $offheapx.Layout(..$fields)"

  def align(fields: Seq[Field], sorted: Boolean, padded: Boolean) = {
    val sortedfields = if (sorted) fields.sortBy(f => sizeOf(f.tpe)).reverse else fields
    var lastoffset = 0L
    val offsetMap = sortedfields.map { f =>
      val tpealignment = alignmentOf(f.tpe)
      val padding =
        if (!padded | lastoffset % tpealignment == 0) 0
        else tpealignment - lastoffset % tpealignment
      val offset = lastoffset + padding
      lastoffset = offset + sizeOf(f.tpe)
      (f, offset)
    }.toMap
    fields.map { f => f.copy(offset = offsetMap(f)) }
  }

  def unaligned(pairs: Tree*) = wrap(align(unwrap(pairs), sorted = false, padded = false))
  def aligned(pairs: Tree*)   = wrap(align(unwrap(pairs), sorted = false, padded = true))
  def packed(pairs: Tree*)    = wrap(align(unwrap(pairs), sorted = true,  padded = true))
}
