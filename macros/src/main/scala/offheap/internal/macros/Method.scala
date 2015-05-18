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

  def access(addr: Tree, f: Field) =
    if (f.isData) {
      val companion = f.tpe.typeSymbol.companion
      q"$companion.fromAddr($addr + ${f.offset})"
    } else
      read(q"$addr + ${f.offset}", f.tpe)

  def accessor[C: WeakTypeTag, T: WeakTypeTag](addr: Tree, name: Tree): Tree = {
    val C = wt[C]
    assertAllocatable(C)
    val Clazz(clazz) = C
    val q"${nameStr: String}" = name
    clazz.fields.collectFirst {
      case f if f.name.toString == nameStr =>
        nullChecked(addr, access(addr, f))
    }.getOrElse {
      abort(s"$C doesn't have field `$nameStr`")
    }
  }

  def assign(addr: Tree, f: Field, value: Tree) =
    if (f.isData) {
      val companion = f.tpe.typeSymbol.companion
      val from      = validate(q"$companion.toAddr($value)")
      val to        = validate(q"$addr + ${f.offset}")
      val size      = sizeOfData(f.tpe)
      q"$UNSAFE.copyMemory($from, $to, $size)"
    } else write(q"$addr + ${f.offset}", f.tpe, value)

  def assigner[C: WeakTypeTag, T: WeakTypeTag](addr: Tree, name: Tree, value: Tree) = {
    val C = wt[C]
    assertAllocatable(C)
    val Clazz(clazz) = C
    val q"${nameStr: String}" = name
    clazz.fields.collectFirst {
      case f if f.name.toString == nameStr =>
        nullChecked(addr, assign(addr, f, value))
    }.getOrElse {
      abort(s"$C doesn't have field `$nameStr`")
    }
  }

  // TODO: zero fields by default
  def allocator[C: WeakTypeTag](alloc: Tree, args: Tree*): Tree = {
    val C = wt[C]
    val Clazz(clazz) = C
    val tagValueOpt = clazz.tag.map(_.value)
    val addr = fresh("addr")
    val size =
      if (clazz.fields.isEmpty) q"1"
      else q"$offheap.sizeOfData[$C]"
    val writes = clazz.fields.zip(tagValueOpt ++: args).map { case (f, arg) =>
      assign(q"$addr", f, arg)
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
    val Clazz(clazz) = C
    val actualFields = if (clazz.tag.isEmpty) clazz.fields else clazz.fields.tail
    val sb = fresh("sb")
    val appends =
      if (actualFields.isEmpty) Nil
      else actualFields.flatMap { f =>
        List(q"$sb.append($self.${TermName(f.name)})", q"""$sb.append(", ")""")
      }.init
    val path = (C :: clazz.parents).reverse.map(_.typeSymbol.name.toString).mkString("", ".", "")
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
