package scala.offheap
package internal
package macros

trait Common extends Definitions {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._
  import c.internal._, decorators._

  def abort(msg: String, at: Position = c.enclosingPosition): Nothing = c.abort(at, msg)

  def panic(msg: String = ""): Nothing = abort(s"panic: $msg")

  def unreachable = panic("unreachable")

  def debug[T](header: String)(f: => T): T = {
    val res = f
    println(s"$header = $res")
    res
  }

  def fresh(pre: String): TermName = TermName(c.freshName(pre))

  class SemiStable

  def freshVal(pre: String, tpe: Type, value: Tree, flags: FlagSet = NoFlags): ValDef = {
    val name = fresh(pre)
    val sym = enclosingOwner.newTermSymbol(name).setFlag(flags).setInfo(tpe)
    sym.updateAttachment(new SemiStable)
    val vd = valDef(sym, value)
    vd
  }

  def freshVar(pre: String, tpe: Type, value: Tree): ValDef =
    freshVal(pre, tpe, value, flags = Flag.MUTABLE)

  /** Extension to default type unlifting that also handles
   *  literal constant types produced after typechecking of classOf.
   */
  implicit object UnliftType extends Unliftable[Type] {
    def unapply(t: Tree) = t match {
      case Literal(Constant(tpe: Type)) =>
        Some(tpe)
      case tt: TypeTree if tt.tpe != null =>
        Some(tt.tpe)
      case q"${m: RefTree}.classOf[${tpe: Type}]" if m.symbol == PredefModule =>
        Some(tpe)
      case _ =>
        None
    }
  }

  object TupleOf {
    def unapply(tpe: Type): Option[List[Type]] =
      if (tpe.typeSymbol == UnitClass) Some(Nil)
      else TupleClass.seq.find(_ == tpe.typeSymbol).map(sym => tpe.baseType(sym).typeArgs)
  }

  object Primitive {
    def unapply(tpe: Type): Boolean = tpe.typeSymbol match {
      case sym: ClassSymbol if sym.isPrimitive && sym != UnitClass => true
      case _                                                       => false
    }
  }

  object Allocatable {
    def unapply(tpe: Type): Boolean = tpe match {
      case Primitive() => true
      case _           => false
    }
  }

  def sizeOf(tpe: Type): Long = tpe match {
    case ByteTpe  | BooleanTpe => 1
    case ShortTpe | CharTpe    => 2
    case IntTpe   | FloatTpe   => 4
    case LongTpe  | DoubleTpe  => 8
    case _                     => abort(s"can't compute size of $tpe")
  }

  def alignmentOf(tpe: Type) = tpe match {
    case ByteTpe  | BooleanTpe => 1
    case ShortTpe | CharTpe    => 2
    case IntTpe   | FloatTpe   => 4
    case LongTpe  | DoubleTpe  => 8
    case _                     => abort(s"can't compute alignment for $tpe")
  }

  def read(addr: Tree, tpe: Type): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val getT = TermName(s"get$tpe")
      q"$MemoryModule.$getT($addr)"
    case BooleanTpe =>
      q"$MemoryModule.getByte($addr) != ${Literal(Constant(0.toByte))}"
  }

  def write(addr: Tree, tpe: Type, value: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val putT = TermName(s"put$tpe")
      q"$MemoryModule.$putT($addr, $value)"
    case BooleanTpe =>
      q"""
        $MemoryModule.putByte($addr,
                              if ($value) ${Literal(Constant(1.toByte))}
                              else ${Literal(Constant(0.toByte))})
      """
  }

  def isSemiStable(sym: Symbol) =
    (sym.isTerm && sym.asTerm.isStable) || sym.attachments.get[SemiStable].nonEmpty

  // TODO: handle non-function literal cases
  def app(f: Tree, argValues: Tree*) = f match {
    case q"(..$params => $body)" =>
      changeOwner(body, f.symbol, enclosingOwner)
      val args = params zip(argValues) map { case (param, argValue) =>
        val q"$_ val $_: $argTpt = $_" = param
        argValue match {
          case rt: RefTree if isSemiStable(rt.symbol) =>
            (rt, q"")
          case _ =>
            val vd = freshVal("arg", argTpt.tpe, argValue)
            (q"${vd.symbol}", vd)
        }
      }
      val argVals = args map (_._1)
      val argDefs = args map (_._2) filter { case q"" => false; case _ => true }

      val param2Val = params zip (argVals) map { case (param, arg) =>
        (param.symbol, arg)
      } toMap

      val transformedBody = typingTransform(body) { (tree, api) =>
        tree match {
          case id: Ident if param2Val.contains(id.symbol) =>
            val arg = param2Val(id.symbol)
            api.typecheck(q"$arg")
          case _ =>
            api.default(tree)
        }
      }

      q"..$argDefs; $transformedBody"
    case _             =>
      q"$f(..$argValues)"
  }

  def stabilized(tree: Tree)(f: Tree => Tree) = tree match {
    case q"${const: Literal}" =>
      f(const)
    case q"${refTree: RefTree}" if isSemiStable(refTree.symbol) =>
      f(refTree)
    case _ =>
      if (tree.tpe == null) {
        val stable = fresh("stable")
        q"val $stable = $tree; ${f(q"$stable")}"
      } else {
        val stable = freshVal("stable", tree.tpe, tree)
        val fapp = f(q"${stable.symbol}")
        q"$stable; $fapp"
      }
  }

  def paramTpe(tpe: Type): Type = tpe.typeArgs.head
  def paramTpe(t: Tree): Type   = paramTpe(t.tpe)

  def assertAllocatable(T: Type, msg: String = ""): Unit = T match {
    case Allocatable() => ()
    case _             => abort(if (msg.isEmpty) s"$T is not allocatable" else msg)
  }

  def isNull(addr: Tree)  = q"$addr == 0L"
  def notNull(addr: Tree) = q"$addr != 0L"

  def classOf(tpt: Tree) = q"$PredefModule.classOf[$tpt]"

  def padded(base: Long, alignment: Long) =
    if (base % alignment == 0) base
    else base + (alignment - base % alignment)

  def flatten(trees: List[Tree]) =
    trees.reduceOption { (l, r) => q"..$l; ..$r" }.getOrElse(q"")

  implicit def flags2long(flags: FlagSet) = flags.asInstanceOf[Long]
  implicit def long2flags(flags: Long)    = flags.asInstanceOf[FlagSet]
}
