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
      val from      = q"$companion.toAddr($value)"
      val to        = q"$addr + ${f.offset}"
      val size      = sizeOfData(f.tpe)
      q"$MemoryModule.copy($from, $to, $size)"
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

  def flatten(trees: List[Tree]) =
    trees.reduceOption { (l, r) => q"..$l; ..$r" }.getOrElse(q"")

  def initialize(clazz: Clazz, addr: TermName, args: Seq[Tree],
                 discardResult: Boolean, prezeroed: Boolean): Tree = {
    val (preamble, zeroed) =
      if (!clazz.hasInit) (q"", prezeroed)
      else if (clazz.fields.filter(_.inBody).isEmpty || prezeroed) (q"", prezeroed)
      else (q"$MemoryModule.zero($addr, ${clazz.size})", true)
    val values = clazz.tag.map(_.value) ++: args
    val writes = clazz.fields.zip(values).map { case (f, v) =>
      v match {
        case Allocation(fclazz, fargs, _) if f.isData =>
          def init(faddr: TermName) =
            initialize(fclazz, faddr, fargs, discardResult = true, prezeroed = zeroed)
          if (f.offset == 0)
            init(addr)
          else {
            val faddr = fresh("addr")
            q"""
              val $faddr = $addr + ${f.offset}
              ..${init(faddr)}
            """
          }
        case _ =>
          assign(q"$addr", f, v)
      }
    }
    val newC = q"${clazz.companion}.fromAddr($addr)"
    val instantiated =
      if (clazz.hasInit) q"$newC.$initializer"
      else if (discardResult) q""
      else newC
    q"""
      ..$preamble
      ..${flatten(writes)}
      ..$instantiated
    """
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
