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
  object ExtractParent             extends ExtractAnnotation(ParentClass)
  object ExtractPotentialChildren  extends ExtractAnnotation(PotentialChildrenClass)
  object ExtractClassTag           extends ExtractAnnotation(ClassTagClass)
  object ExtractClassTagRange      extends ExtractAnnotation(ClassTagRangeClass)
  object ExtractParentExtractor    extends ExtractAnnotation(ParentExtractorClass)
  object ExtractPrimaryExtractor   extends ExtractAnnotation(PrimaryExtractorClass)
  object ExtractUniversalExtractor extends ExtractAnnotation(UniversalExtractorClass)
  object ExtractField              extends ExtractAnnotation(FieldClass)

  final case class Tag(value: Tree, tpt: Tree)

  case class Field(name: String, after: Tree, tpe: Type,
                   annots: List[Tree], offset: Long) {
    lazy val isEmbed   = annots.collect { case q"new $c" if c.symbol == EmbedClass => c }.nonEmpty
    lazy val inCtor    = annots.collect { case q"new $c" if c.symbol == CtorClass => c }.nonEmpty
         val inBody    = !inCtor
    lazy val size      = if (isEmbed) sizeOfEmbed(tpe) else sizeOf(tpe)
    lazy val alignment = if (isEmbed) alignmentOfEmbed(tpe) else alignmentOf(tpe)
  }
  object Field {
    implicit val lift: Liftable[Field] = Liftable { f =>
      q"""
        new $FieldClass[${f.tpe}](
          ${f.name}, ${f.after},
          new $AnnotsClass(..${f.annots}), (${f.offset}: $SizeTpe))
      """
    }
    implicit val unlift: Unliftable[Field] = Unliftable {
      case q"""
        new ${ftpe: Type}(
          ${name: String}, $after, $newAnnots, (${offset: Long}: $_))
        """
        if ftpe.typeSymbol == FieldClass =>
        val tpe = ftpe.baseType(FieldClass).typeArgs.head
        val annots = newAnnots match {
          case q"new $_(..$anns)" => anns
          case q"new $_"          => Nil
        }
        Field(name, after, tpe, annots, offset)
    }
  }

  final case class Clazz(sym: Symbol) {
    lazy val tpe = sym.asType.toType
    lazy val hasInit = tpe.members.find(_.name == initializer).nonEmpty
    lazy val companion = tpe.typeSymbol.companion
    lazy val fields =
      sym.asType.toType.members.collect {
        case ExtractField(t :: Nil) =>
          val q"${f: Field}" = t
          f
      }.toList.sortBy(_.offset)
    lazy val actualFields = if (tag.isEmpty) fields else fields.tail
    lazy val parents =
      ExtractParent.unapply(sym).toList.flatten.map {
        case q"new $_(${tpe: Type})" => tpe
      }
    lazy val tag =
      ExtractClassTag.unapply(sym).map(_.head).map {
        case q"new $_($value: $tpt)" => Tag(value, tpt)
      }
    lazy val isData = ExtractData.unapply(sym).nonEmpty
    lazy val isEnum = ExtractEnum.unapply(sym).nonEmpty
    lazy val children: List[Clazz] =
      ExtractPotentialChildren.unapply(sym).map {
        case q"new $_(..${tpes: List[Type]})" :: Nil =>
          tpes.map(Clazz.unapply).flatten
      }.getOrElse(Nil)
    lazy val size: Long = {
      assertLayoutComplete(sym, s"$sym must be defined before it's used")
      if (fields.isEmpty) 1L
      else if (isData) {
        val lastfield = fields.maxBy(_.offset)
        lastfield.offset + lastfield.size
      } else if (isEnum) {
        children.map(_.size).max
      } else unreachable
    }
    lazy val alignment: Long = {
      assertLayoutComplete(sym, s"$sym must be defined before it's used")
      if (fields.isEmpty) 1L
      else if (isData) {
        fields.map(f => alignmentOf(f.tpe)).max
      } else if (isEnum) {
        children.map(_.alignment).max
      } else unreachable
    }
  }
  object Clazz {
    final case class Attachment(value: Boolean)
    final case class InLayout()
    final case class LayoutComplete()
    def is(tpe: Type): Boolean =
      is(tpe.widen.typeSymbol)
    def is(sym: Symbol): Boolean = {
      sym.attachments.get[Clazz.Attachment].map { _.value }.getOrElse {
        val value =
          ExtractData.unapply(sym).nonEmpty ||
          ExtractEnum.unapply(sym).nonEmpty
        sym.updateAttachment(Clazz.Attachment(value))
        value
      }
    }
    def unapply(tpe: Type): Option[Clazz] =
      unapply(tpe.widen.typeSymbol)
    def unapply(sym: Symbol): Option[Clazz] =
      if (is(sym)) Some(Clazz(sym))
      else None
  }

  object ArrayOf {
    def is(tpe: Type) = isEmbed(tpe) || isNonEmbed(tpe)
    def isNonEmbed(tpe: Type) = tpe.typeSymbol == ArrayClass
    def isEmbed(tpe: Type) = tpe.typeSymbol == EmbedArrayClass
    def unapply(tpe: Type): Option[(Type, Boolean)] =
      if (isEmbed(tpe)) Some((paramTpe(tpe), true))
      else if (isNonEmbed(tpe)) Some((paramTpe(tpe), false))
      else None
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
      case Primitive() | Clazz(_) => true
      case _                      => false
    }
  }

  def sizeOf(tpe: Type): Long = tpe match {
    case ByteTpe  | BooleanTpe => 1
    case ShortTpe | CharTpe    => 2
    case IntTpe   | FloatTpe   => 4
    case LongTpe  | DoubleTpe  => 8
    case _ if Clazz.is(tpe)    ||
              ArrayOf.is(tpe)  => 8
    case _                     => abort(s"can't compute size of $tpe")
  }

  def sizeOfEmbed(tpe: Type): Long = tpe match {
    case Clazz(clazz) => clazz.size
    case _            => abort(s"$tpe is not an offheap class")
  }

  def alignmentOf(tpe: Type) = tpe match {
    case ByteTpe  | BooleanTpe => 1
    case ShortTpe | CharTpe    => 2
    case IntTpe   | FloatTpe   => 4
    case LongTpe  | DoubleTpe  => 8
    case _ if Clazz.is(tpe)    ||
              ArrayOf.is(tpe)  => 8
    case _                     => abort(s"can't compute alignment for $tpe")
  }

  def alignmentOfEmbed(tpe: Type) = tpe match {
    case Clazz(clazz) => clazz.alignment
    case _            => abort(s"$tpe is not an offheap class")
  }

  def read(addr: Tree, tpe: Type): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val getT = TermName(s"get$tpe")
      q"$MemoryModule.$getT($addr)"
    case BooleanTpe =>
      q"$MemoryModule.getByte($addr) != ${Literal(Constant(0.toByte))}"
    case ArrayOf(tpe, isEmbed) =>
      val module = if (isEmbed) EmbedArrayModule else ArrayModule
      q"$module.fromAddr[$tpe]($MemoryModule.getLong($addr))"
    case Clazz(_) =>
      val companion = tpe.typeSymbol.companion
      q"$companion.fromAddr($MemoryModule.getLong($addr))"
  }

  def readEmbed(addr: Tree, tpe: Type): Tree = {
    val companion = tpe.typeSymbol.companion
    q"$companion.fromAddr($addr)"
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
    case Clazz(_) | ArrayOf(_, _) =>
      q"$MemoryModule.putLong($addr, $value.addr)"
  }

  def writeEmbed(addr: Tree, tpe: Type, value: Tree) = value match {
    case Allocation(clazz, args, _) =>
      val naddr = fresh("naddr")
      q"""
        val $naddr = $addr
        ..${initialize(clazz, naddr, args, discardResult = true, prezeroed = false)}
      """
    case _ =>
      val from = fresh("from")
      val size = sizeOfEmbed(tpe)
      q"""
        val $from = $value.addr
        ${nullChecked(q"$from", q"$MemoryModule.copy($from, $addr, $size)")}
      """
  }

  def isSemiStable(sym: Symbol) =
    (sym.isTerm && sym.asTerm.isStable) || sym.attachments.get[SemiStable].nonEmpty

  // TODO: handle non-function literal cases
  def appSubs(f: Tree, argValue: Tree, subs: Tree => Tree) = f match {
    case q"($param => $body)" =>
      val q"$_ val $_: $argTpt = $_" = param
      changeOwner(body, f.symbol, enclosingOwner)
      val (arg, argDef) = argValue match {
        case rt: RefTree if isSemiStable(rt.symbol) =>
          (rt, q"")
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

  def assertEmbeddable(T: Type): Unit = T match {
    case Clazz(_) => ()
    case _        => abort(s"$T can not be embedded as it is not an offheap class")
  }

  def assertNotInLayout(sym: Symbol, msg: String) =
    if (sym.attachments.get[Clazz.InLayout].nonEmpty)
      abort(msg)

  def assertLayoutComplete(sym: Symbol, msg: String) =
    if (sym.attachments.get[Clazz.LayoutComplete].isEmpty) {
      abort(msg)
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
    val toCompanion = to.typeSymbol.companion
    q"$toCompanion.fromAddr($v.addr)"
  }

  def isNull(addr: Tree)  = q"$addr == 0L"
  def notNull(addr: Tree) = q"$addr != 0L"

  def classOf(tpt: Tree) = q"$PredefModule.classOf[$tpt]"

  def padded(base: Long, alignment: Long) =
    if (base % alignment == 0) base
    else base + (alignment - base % alignment)

  def nullChecked(addr: Tree, ifOk: Tree) =
    q"""
      if ($CheckedModule.NULL)
        if ($addr == 0L) throw new $NullPointerExceptionClass
      $ifOk
    """

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
      if (clazz.fields.filter(_.inBody).isEmpty || prezeroed) (q"", prezeroed)
      else (q"$MemoryModule.zero($addr, ${clazz.size})", true)
    val values = clazz.tag.map(_.value) ++: args
    val writes = clazz.fields.zip(values).map { case (f, v) =>
      assign(q"$addr", f, v)
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

  def access(addr: Tree, f: Field) =
    if (f.isEmbed) readEmbed(q"$addr + ${f.offset}", f.tpe)
    else read(q"$addr + ${f.offset}", f.tpe)

  def assign(addr: Tree, f: Field, value: Tree) =
    if (f.isEmbed) writeEmbed(q"$addr + ${f.offset}", f.tpe, value)
    else write(q"$addr + ${f.offset}", f.tpe, value)

  def strideOf(T: Type, isEmbed: Boolean): Long =
    if (!isEmbed) sizeOf(T)
    else padded(sizeOfEmbed(T), alignmentOfEmbed(T))

  implicit def flags2long(flags: FlagSet) = flags.asInstanceOf[Long]
  implicit def long2flags(flags: Long)    = flags.asInstanceOf[FlagSet]
}
