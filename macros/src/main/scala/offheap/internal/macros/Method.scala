package scala.offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Method(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._
  import c.internal._, decorators._

  def accessor[C: WeakTypeTag, T: WeakTypeTag](self: Tree, name: Tree): Tree = {
    val C = wt[C]
    assertAllocatable(C)
    val Clazz(clazz) = C
    val q"${nameStr: String}" = name
    clazz.fields.collectFirst {
      case f if f.name.toString == nameStr =>
        nullChecked(q"$self.addr", access(q"$self.addr", f))
    }.getOrElse {
      abort(s"$C doesn't have field `$nameStr`")
    }
  }

  def assigner[C: WeakTypeTag, T: WeakTypeTag](self: Tree, name: Tree, value: Tree) = {
    val C = wt[C]
    assertAllocatable(C)
    val Clazz(clazz) = C
    val q"${nameStr: String}" = name
    clazz.fields.collectFirst {
      case f if f.name.toString == nameStr =>
        nullChecked(q"$self.addr", assign(q"$self.addr", f, value))
    }.getOrElse {
      abort(s"$C doesn't have field `$nameStr`")
    }
  }

  def allocate(anyC: Any, anyArgs: Any, anyAlloc: Any): Tree = {
    val C = anyC.asInstanceOf[Type]
    val args = anyArgs.asInstanceOf[List[Tree]]
    val alloc = anyAlloc.asInstanceOf[Tree]
    val Clazz(clazz) = C
    val addr = fresh("addr")
    Allocation(clazz, args, alloc, q"""
      val $addr = $alloc.allocate(${clazz.size})
      ..${initialize(clazz, addr, args, discardResult = false, prezeroed = false)}
    """)
  }

  def toString[C: WeakTypeTag](self: Tree): Tree = {
    val C = wt[C]
    val Clazz(clazz) = C
    val sb = fresh("sb")
    val appends =
      if (clazz.actualFields.isEmpty) Nil
      else clazz.actualFields.flatMap { f =>
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
