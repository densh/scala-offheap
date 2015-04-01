package offheap
package internal
package macros

trait Common extends Definitions {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  def abort(msg: String, at: Position = c.enclosingPosition): Nothing = c.abort(at, msg)

  def panic(msg: String = ""): Nothing = abort(s"panic: $msg")

  def unreachable = panic("unreachable")

  def debug[T](header: String)(f: => T): T = {
    val res = f
    println(s"$header = $res")
    res
  }

  def fresh(pre: String): TermName = TermName(c.freshName(pre))

  def freshVal(pre: String, tpe: Type, value: Tree): ValDef = {
    import c.internal._, c.internal.decorators._
    val name = fresh(pre)
    val sym = enclosingOwner.newTermSymbol(name).setInfo(tpe)
    val vd = valDef(sym, value)
    vd
  }

  case class Field(name: String, tpe: Type, offset: Long)
  object Field {
    implicit val lift: Liftable[Field] = Liftable { f =>
      q"(${f.name}, $PredefModule.classOf[${f.tpe}], ${f.offset})"
    }
    implicit val unlift: Unliftable[Field] = Unliftable {
      case q"(${s: String}, ${t: Type}, ${offset: Long})" =>
        Field(s, t, offset)
    }
  }

  /** Extension to default type unlifting that also handles
   *  literal constant types produced by classOf.
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

  class ExtractAnnotation(annSym: Symbol) {
    def unapply(sym: Symbol): Option[List[Tree]] = {
      val trees = sym.annotations.collect {
        case ann if ann.tpe.typeSymbol == annSym => ann.tree
      }
      if (trees.isEmpty) None else Some(trees)
    }
  }
  object ExtractEnum               extends ExtractAnnotation(EnumClass)
  object ExtractData               extends ExtractAnnotation(DataClass)
  object ExtractLayout             extends ExtractAnnotation(LayoutClass)
  object ExtractParent             extends ExtractAnnotation(ParentClass)
  object ExtractClassTag           extends ExtractAnnotation(ClassTagClass)
  object ExtractClassTagRange      extends ExtractAnnotation(ClassTagRangeClass)
  object ExtractParentExtractor    extends ExtractAnnotation(ParentExtractorClass)
  object ExtractPrimaryExtractor   extends ExtractAnnotation(PrimaryExtractorClass)
  object ExtractUniversalExtractor extends ExtractAnnotation(UniversalExtractorClass)
  object ExtractUnchecked          extends ExtractAnnotation(UncheckedClass)

  object ClassOf {
    def unapply(tpe: Type): Option[(List[Field], List[Type], Option[(Tree, Tree)])] =
      unapply(tpe.widen.typeSymbol)
    def unapply(sym: Symbol): Option[(List[Field], List[Type], Option[(Tree, Tree)])] = {
      val fieldsOpt: Option[List[Field]] =
        ExtractLayout.unapply(sym).map { layouts =>
          layouts.head match {
            case q"new $_(..${fields: List[Field]})" => fields
            case q"new $_"                           => Nil
          }
        }
      fieldsOpt.map { fields =>
        val parents = ExtractParent.unapply(sym).toList.flatten.map {
          case q"new $_(${tpe: Type})" => tpe
        }
        val tagOpt = ExtractClassTag.unapply(sym).map(_.head).map {
          case q"new $_($value: $tpt)" => (value, tpt)
        }
        (fields, parents, tagOpt)
      }
    }
  }

  object ArrayOf {
    def unapply(tpe: Type): Option[Type] =
      if (tpe.typeSymbol != ArrayClass) None
      else Some(paramTpe(tpe))
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
      case Primitive() | ClassOf(_, _, _) => true
      case _                              => false
    }
  }

  def sizeOf(tpe: Type): Long = tpe match {
    case ByteTpe  | BooleanTpe         => 1
    case ShortTpe | CharTpe            => 2
    case IntTpe   | FloatTpe           => 4
    case LongTpe  | DoubleTpe          => 8
    case ClassOf(_, _, _) | ArrayOf(_) => if (bitDepth == 64) 12 else 8
    case _                             => abort(s"can't compute size of $tpe")
  }

  def sizeOfData(tpe: Type): Long = tpe match {
    case ClassOf(fields, _, _) =>
      val lastfield = fields.maxBy(_.offset)
      lastfield.offset + sizeOf(lastfield.tpe)
    case _ =>
      abort(s"$tpe is not a an offheap class")
  }

  def alignment(tpe: Type) = tpe match {
    case ByteTpe  | BooleanTpe                   => 1
    case ShortTpe | CharTpe                      => 2
    case IntTpe   | FloatTpe                     => 4
    case LongTpe  | DoubleTpe | ClassOf(_, _, _) => 8
    case _                                       => abort(s"can't comput alignment for $tpe")
  }

  def read(addr: Tree, tpe: Type, memory: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val getT = TermName(s"get$tpe")
      q"$memory.$getT($addr)"
    case BooleanTpe =>
      q"$memory.getByte($addr) != ${Literal(Constant(0.toByte))}"
    case ArrayOf(tpe) =>
      q"$ArrayModule.fromRef[$tpe]($memory.getRef($addr))"
    case ClassOf(_, _, _) =>
      val companion = tpe.typeSymbol.companion
      val getRef = if (checked) TermName("getRef") else TermName("getLong")
      q"$companion.fromRef($memory.$getRef($addr))"
  }

  def write(addr: Tree, tpe: Type, value: Tree, memory: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val putT = TermName(s"put$tpe")
      q"$memory.$putT($addr, $value)"
    case BooleanTpe =>
      q"""
        $memory.putByte($addr,
                        if ($value) ${Literal(Constant(1.toByte))}
                        else ${Literal(Constant(0.toByte))})
      """
    case ArrayOf(_) =>
      q"$memory.putRef($addr, $ArrayModule.toRef($value))"
    case ClassOf(_, _, _) =>
      val companion = tpe.typeSymbol.companion
      val putRef = if (checked) TermName("putRef") else TermName("putLong")
      q"$memory.$putRef($addr, $companion.toRef($value))"
  }


  // TODO: handle non-function literal cases
  def appSubs(f: Tree, argValue: Tree, subs: Tree => Tree) = f match {
    case q"($param => $body)" =>
      import c.internal._, c.internal.decorators._
      val q"$_ val $_: $argTpt = $_" = param
      changeOwner(body, f.symbol, enclosingOwner)
      val (arg, argDef) = argValue match {
        case refTree: RefTree
          if refTree.symbol.isTerm
          && refTree.symbol.asTerm.isStable =>
          (refTree, q"")
        case _ =>
          val vd = freshVal("arg", argTpt.tpe, argValue)
          (q"${vd.symbol}", vd)
      }
      val transformedBody = typingTransform(body) { (tree, api) =>
        tree match {
          case id: Ident if id.symbol == param.symbol =>
            api.typecheck(subs(q"$arg"))
          case _ =>
            api.default(tree)
        }
      }
      q"..$argDef; $transformedBody"
    case _             =>
      q"$f($argValue)"
  }

  def app(f: Tree, argValue: Tree) =
    appSubs(f, argValue, identity)

  def stabilized(tree: Tree)(f: Tree => Tree) = tree match {
    case q"${refTree: RefTree}"
      if refTree.symbol.isTerm
      && refTree.symbol.asTerm.isStable =>
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

  def assertAllocatable(T: Type, msg: String = ""): Unit =
    T match {
      case Allocatable() => ()
      case _             => abort(if (msg.isEmpty) s"$T is not allocatable" else msg)
    }

  def isEnum(T: Type): Boolean = ExtractEnum.unapply(T.typeSymbol).nonEmpty

  def isData(T: Type): Boolean = ExtractData.unapply(T.typeSymbol).nonEmpty

  def isRelated(T: Type, C: Type): Boolean = {
    def topmostParent(sym: Symbol): Symbol =
      ExtractParent.unapply(sym).map {
        case _ :+ q"new $_(${tpe: Type})" => tpe.typeSymbol
      }.getOrElse(sym)
    topmostParent(T.typeSymbol) == topmostParent(C.typeSymbol)
  }

  def isParent(T: Type, C: Type): Boolean =
    ExtractParent.unapply(C.typeSymbol).getOrElse(Nil).exists {
      case q"new $_(${tpe: Type})" => tpe.typeSymbol == T.typeSymbol
      case _                       => false
    }

  def cast(v: Tree, from: Type, to: Type) = {
    val fromCompanion = from.typeSymbol.companion
    val toCompanion = to.typeSymbol.companion
    q"$toCompanion.fromRef($fromCompanion.toRef($v))"
  }
}
