package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Method(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  def nullChecked(addr: Tree, ifOk: Tree) =
    q"""
      if ($CHECKED)
        if ($addr == 0L) throw new $NullPointerExceptionClass
      $ifOk
    """

  def accessor[C: WeakTypeTag, T: WeakTypeTag](addr: Tree, name: Tree): Tree = {
    val C = wt[C]
    assertAllocatable(C)
    val ClassOf(fields, _, _) = C
    val q"${nameStr: String}" = name
    fields.collectFirst {
      case f if f.name.toString == nameStr =>
        nullChecked(addr, read(q"$addr + ${f.offset}", f.tpe, memory(addr)))
    }.getOrElse {
      abort(s"$C doesn't have field `$nameStr`")
    }
  }

  def assigner[C: WeakTypeTag, T: WeakTypeTag](addr: Tree, name: Tree, value: Tree) = {
    val C = wt[C]
    assertAllocatable(C)
    val ClassOf(fields, _, _) = C
    val q"${nameStr: String}" = name
    fields.collectFirst {
      case f if f.name.toString == nameStr =>
        nullChecked(addr, write(q"$addr + ${f.offset}", f.tpe, value, memory(addr)))
    }.getOrElse {
      abort(s"$C doesn't have field `$nameStr`")
    }
  }

  // TODO: zero fields by default
  // TODO: zero-size data structures should not allocate any memory
  def allocator[C: WeakTypeTag](alloc: Tree, args: Tree*): Tree = {
    val C = wt[C]
    val ClassOf(fields, _, tagOpt) = C
    val tagValueOpt = tagOpt.map { case (v, tpt) => v }
    val addr = fresh("addr")
    val size =
      if (fields.isEmpty) q"1"
      else q"$offheap.sizeOfData[$C]"
    val writes = fields.zip(tagValueOpt ++: args).map { case (f, arg) =>
      write(q"$addr + ${f.offset}", f.tpe, arg, memory(q"$addr"))
    }
    val newC = q"new $C($addr)"
    val instantiate = C.members.find(_.name == initialize).map { _ =>
      val instance = fresh("instance")
      q"""
        val $instance = $newC
        $instance.$initialize
        $instance
      """
    }.getOrElse(newC)
    q"""
      val $addr = $alloc.allocate($size)
      ..$writes
      ..$instantiate
    """
  }

  def toString[C: WeakTypeTag](self: Tree): Tree = {
    val C = wt[C]
    val ClassOf(fields, parents, tagOpt) = C
    val actualFields = if (tagOpt.isEmpty) fields else fields.tail
    val sb = fresh("sb")
    val appends =
      if (actualFields.isEmpty) Nil
      else actualFields.flatMap { f =>
        List(q"$sb.append($self.${TermName(f.name)})", q"""$sb.append(", ")""")
      }.init
    val path = (C :: parents).reverse.map(_.typeSymbol.name.toString).mkString("", ".", "")
    val companion = C.typeSymbol.companion
    q"""
      val $sb = new $StringBuilderClass
      $sb.append($path)
      if ($self == $companion.empty)
        $sb.append(".empty")
      else {
        $sb.append("(")
        ..$appends
        $sb.append(")")
      }
      $sb.toString
    """
  }

  def is[C: WeakTypeTag, T: WeakTypeTag]: Tree = {
    val C = wt[C]
    val T = wt[T]
    if (!isRelated(C, T)) q"false"
    else if (C =:= T) q"true"
    else {
      val tg = fresh("tag")
      val check =
        if (isEnum(T)) {
          val ExtractClassTagRange(q"new $_($from: $_, $to: $_)" :: Nil) = T.typeSymbol
          q"$tg > $from && $tg <= $to"
        } else if (isData(T)) {
          val ExtractClassTag(q"new $_($value: $_)" :: Nil) = T.typeSymbol
          q"$tg == $value"
        } else unreachable
      q"""
        val $tg = ${c.prefix}.$tag
        $check
      """
    }
  }

  def as[C: WeakTypeTag, T: WeakTypeTag]: Tree = {
    def fail = q"throw new $offheap.CastException"
    def ok   = cast(c.prefix.tree, wt[C], wt[T])
    is[C, T] match {
      case q"${cond: Boolean}" => if (cond) ok else fail
      case cond                => q"if ($cond) $ok else $fail"
    }
  }
}
