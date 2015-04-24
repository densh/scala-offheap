package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Method(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  def throwNullRef = q"throw new _root_.java.lang.NullPointerException"

  def nullChecked(ref: Tree, ifOk: Tree) =
    if (checked) ifOk
    else q"""
      if (${isNull(ref)}) throw new $NullPointerExceptionClass
      else $ifOk
    """

  def accessor[C: WeakTypeTag, T: WeakTypeTag](ref: Tree, name: Tree): Tree = {
    val C = wt[C]
    assertAllocatable(C)
    val ClassOf(fields, _, _) = C
    val q"${nameStr: String}" = name
    fields.collectFirst {
      case f if f.name.toString == nameStr =>
        nullChecked(ref, read(q"${addr(ref)} + ${f.offset}", f.tpe, memory(ref)))
    }.getOrElse {
      abort(s"$C ($fields) doesn't have field `$nameStr`")
    }
  }

  def assigner[C: WeakTypeTag, T: WeakTypeTag](ref: Tree, name: Tree, value: Tree) = {
    val C = wt[C]
    assertAllocatable(C)
    val ClassOf(fields, _, _) = C
    val q"${nameStr: String}" = name
    fields.collectFirst {
      case f if f.name.toString == nameStr =>
        nullChecked(ref, write(q"${addr(ref)} + ${f.offset}", f.tpe, value, memory(ref)))
    }.getOrElse {
      abort(s"$C ($fields) doesn't have field `$nameStr`")
    }
  }

  // TODO: zero fields by default
  // TODO: zero-size data structures should not allocate any memory
  def allocator[C: WeakTypeTag](memory: Tree, args: Tree*): Tree = {
    val C = wt[C]
    val ClassOf(fields, _, tagOpt) = C
    val checked = ExtractUnchecked.unapply(C.typeSymbol).isEmpty
    val tagValueOpt = tagOpt.map { case (v, tpt) => v }
    val addr = fresh("addr")
    val size =
      if (fields.isEmpty) q"1"
      else q"$offheapx.sizeOfData[$C]"
    val writes = fields.zip(tagValueOpt ++: args).map { case (f, arg) =>
      write(q"$addr + ${f.offset}", f.tpe, arg, memory)
    }
    val newC =
      if (checked) q"new $C(new $RefClass($addr, $memory))"
      else q"new $C($addr)"
    val checkNative =
      if (checked) q""
      else q"""
        if ($memory.isVirtual)
          throw new $IllegalArgumentExceptionClass(
            "Unchecked offheap can only be allocated in native memory.")
      """
    val instantiate = C.members.find(_.name == initialize).map { _ =>
      val instance = fresh("instance")
      q"""
        ..$checkNative
        val $instance = $newC
        $instance.$initialize
        $instance
      """
    }.getOrElse(newC)
    q"""
      val $addr = $memory.allocate($size)
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
