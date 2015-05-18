package offheap
package internal
package macros

import scala.reflect.macros.blackbox

class Method(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._
  import c.internal._, decorators._

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

  object Allocation {
    final case class Attachment(clazz: Clazz, args: List[Tree], alloc: Tree)
    def apply(clazz: Clazz, args: List[Tree], alloc: Tree, result: Tree): Tree =
      result.updateAttachment(Attachment(clazz, args, alloc))
    def unapply(tree: Tree): Option[(Clazz, List[Tree], Tree)] = tree match {
      case q"$inner: $_" =>
        inner.attachments.get[Attachment].map { a => (a.clazz, a.args, a.alloc) }
      case _ =>
        None
    }
  }

  // TODO: zero memory in case of initializer presence
  def initialize(clazz: Clazz, addr: TermName, args: Seq[Tree]): Tree = {
    val instance = fresh("instance")
    val values = clazz.tag.map(_.value) ++: args
    val writes = clazz.fields.zip(values).map { case (f, v) =>
      v match {
        // TODO: check that falloc and alloc are the same
        case Allocation(fclazz, fargs, falloc) if f.isData =>
          val faddr = fresh("addr")
          // TODO: deeply flatten this blocks
          // TODO: discard fromAddr call
          // TODO: +0L
          q"""
            val $faddr = $addr + ${f.offset}
            ..${initialize(fclazz, faddr, fargs)}
          """
        case _ =>
          assign(q"$addr", f, v)
      }
    }
    val newC = q"${clazz.companion}.fromAddr($addr)"
    val instantiated = clazz.tpe.members.find(_.name == initializer).map { _ =>
      q"""
        val $instance = $newC
        $instance.$initializer
        $instance
      """
    }.getOrElse(newC)
    q"""
      ..$writes
      ..$instantiated
    """
  }

  def allocate(anyC: Any, anyArgs: Any, anyAlloc: Any): Tree = {
    val C = anyC.asInstanceOf[Type]
    val args = anyArgs.asInstanceOf[List[Tree]]
    val alloc = anyAlloc.asInstanceOf[Tree]
    val Clazz(clazz) = C
    val size =
      if (clazz.fields.isEmpty) q"1"
      else q"$offheap.sizeOfData[${clazz.tpe}]"
    val addr = fresh("addr")
    Allocation(clazz, args, alloc, q"""
      val $addr = $alloc.allocate($size)
      ..${initialize(clazz, addr, args)}
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
