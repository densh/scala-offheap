package offheap
package internal
package macros

import scala.collection.mutable
import scala.reflect.macros.{whitebox, blackbox}

trait Definitions {
  val c: blackbox.Context

  import c.universe._
  import c.universe.definitions._
  import c.universe.rootMirror._

  val bitDepth: Int = 64
  private val prefix = s"offheap.x$bitDepth"
  val offheapx  = staticPackage(prefix)
  val AddrTpe  = LongClass.toType
  val SizeTpe  = LongClass.toType

  val StringBuilderClass            = staticClass("scala.collection.mutable.StringBuilder")
  val IllegalArgumentExceptionClass = staticClass("java.lang.IllegalArgumentException")

  val RegionClass             = staticClass(s"$prefix.Region")
  val RefClass                = staticClass(s"$prefix.Ref")
  val MemoryClass             = staticClass(s"$prefix.Memory")
  val ArrayClass              = staticClass(s"$prefix.Array")
  val DataClass               = staticClass("offheap.internal.Data")
  val EnumClass               = staticClass("offheap.internal.Enum")
  val LayoutClass             = staticClass("offheap.internal.Layout")
  val TagClass                = staticClass("offheap.internal.Tag")
  val ClassTagClass           = staticClass("offheap.internal.ClassTag")
  val ClassTagRangeClass      = staticClass("offheap.internal.ClassTagRange")
  val ParentClass             = staticClass("offheap.internal.Parent")
  val PrimaryExtractorClass   = staticClass("offheap.internal.PrimaryExtractor")
  val ParentExtractorClass    = staticClass("offheap.internal.ParentExractor")
  val UniversalExtractorClass = staticClass("offheap.internal.UniversalExtractor")

  val MethodModule  = staticModule("offheap.internal.Method")
  val PoolModule    = staticModule(s"$prefix.Pool")
  val ArrayModule   = staticModule(s"$prefix.Array")

  val offheap  = staticPackage("offheap")
  val internal = staticPackage("offheap.internal")

  val initialize   = TermName("$initialize")
  val tag          = TermName("$tag")
  val ref          = TermName("$ref")
  val canUseMacros = TermName("$canUseMacros")
}

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

  case class Field(name: String, tpe: Type)

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

  object ClassOf {
    def unapply(tpe: Type): Option[(List[Field], List[Tree], Option[(Tree, Tree)])] =
      unapply(tpe.widen.typeSymbol)
    def unapply(sym: Symbol): Option[(List[Field], List[Tree], Option[(Tree, Tree)])] = {
      val fieldsOpt: Option[List[Field]] =
        ExtractLayout.unapply(sym).map { layouts =>
          layouts.head match {
            case q"new $_(..$descriptors)" =>
              descriptors.map { case q"(${name: String}, new $_[$tpt]())" =>
                Field(name, tpt.tpe)
              }
            case q"new $_" =>
              Nil
          }
        }
      fieldsOpt.map { fields =>
        val parents = ExtractParent.unapply(sym).toList.flatten.map {
          case q"new $_(new $_[$tpt]())" => tpt
        }
        val tagOpt = ExtractClassTag.unapply(sym).map(_.head).map {
          case q"new $_($value: $tpt)" => (value, tpt)
        }
        (fields, parents, tagOpt)
      }
    }
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

  def read(addr: Tree, tpe: Type, memory: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val getT = TermName(s"get$tpe")
      q"$memory.$getT($addr)"
    case BooleanTpe =>
      q"$memory.getByte($addr) != ${Literal(Constant(0.toByte))}"
    case ClassOf(_, _, _) =>
      val companion = tpe.typeSymbol.companion
      q"$companion.fromRef($memory.getRef($addr))"
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
    case ClassOf(_, _, _) =>
      val companion = tpe.typeSymbol.companion
      q"$memory.putRef($addr, $companion.toRef($value))"
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

  def paramTpe(f: Tree) = f.tpe.typeArgs.head

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
        case _ :+ q"new $_(new $_[$tpt]())" => tpt.tpe.typeSymbol
      }.getOrElse(sym)
    topmostParent(T.typeSymbol) == topmostParent(C.typeSymbol)
  }

  def isParent(T: Type, C: Type): Boolean =
    ExtractParent.unapply(C.typeSymbol).getOrElse(Nil).exists {
      case q"new $_(new $_[$tpt]())" => tpt.tpe.typeSymbol == T.typeSymbol
      case _                         => false
    }

  def cast(v: Tree, from: Type, to: Type) = {
    val fromCompanion = from.typeSymbol.companion
    val toCompanion = to.typeSymbol.companion
    q"$toCompanion.fromRef($fromCompanion.toRef($v))"
  }
}

class Annotations(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._
  import Flag._

  def layout(fields: List[SyntacticField]): Tree = {
    val tuples = fields.map { f =>
      q"(${f.name.toString}, new $TagClass[${f.tpt}]())"
    }
    q"new $LayoutClass(..$tuples)"
  }

  implicit class SyntacticField(vd: ValDef) {
    def name        = vd.name
    def tpt         = vd.tpt
    def default     = vd.rhs
    def isMutable   = vd.mods.hasFlag(MUTABLE)
    def isCtorField = vd.mods.hasFlag(PARAMACCESSOR)
  }

  // TODO: modifiers propagation and checking
  // TODO: hygienic reference to class type from companion?
  // TODO: pattern matching on parent scrutinees
  def dataTransform(clazz: Tree, companion: Tree) = {
    // Parse the input trees
    val q"""
      $rawMods class $name[..$rawTargs] $rawCtorMods(..$rawArgs)
                                                    (...$rawRestArgs)
               extends { ..$rawEarly }
               with ..$rawTraits { $rawSelf => ..$rawStats }
    """ = clazz
    val q"""
      $companionMods object $_
                     extends { ..$companionEarly }
                     with ..$companionParents { $companionSelf => ..$companionStats }
    """ = companion

    // Well-formedness checks
    if (rawTargs.nonEmpty)
      abort("data classes don't support generics", at = rawTargs.head.pos)
    if (rawEarly.nonEmpty)
      abort("data classes don't suport early initializer", at = rawEarly.head.pos)
    if (rawMods.flags != NoFlags)
      abort("data classes may not have any modifiers")
    if (rawCtorMods.flags != NoFlags)
      abort("data classes may not have any constructor modifiers")
    if (rawRestArgs.nonEmpty)
      abort("data classes may not have more than one argument list",
            at = rawRestArgs.head.head.pos)
    rawArgs.headOption.foreach { arg =>
      if (arg.mods.hasFlag(IMPLICIT))
        abort("data classes may not have implicit arguments", at = arg.pos)
    }
    rawTraits.foreach {
      case q"${tq"$ref[..$targs]"}(...$args)" =>
        if (args.nonEmpty || targs.nonEmpty)
          abort("data classes can only inherit from universal traits")
        ref
    }

    // Generate fresh names used in desugaring
    val memory       = fresh("memory")
    val instance     = fresh("instance")
    val scrutinee    = fresh("scrutinee")
    val value        = fresh("value")

    // Process and existing members
    val termName = name.toTermName
    val traits = rawTraits match {
      case tq"$pkg.AnyRef" :: Nil if pkg.symbol == ScalaPackage => Nil
      case other                                                => other
    }
    val parents = rawMods.annotations.collect {
      case q"new $annot(new $_[$tpt]())" if annot.symbol == ParentClass =>
        tpt
    }
    val tagOpt = rawMods.annotations.collectFirst {
      case q"new $annot($value: $tpt)" if annot.symbol == ClassTagClass =>
        (value, tpt)
    }
    val fields = {
      val tagField = tagOpt.map {
        case (value, tpt) => new SyntacticField(q"val $tag: $tpt")
      }.toList
      def checkMods(mods: Modifiers) =
        if (mods.hasFlag(LAZY))
          abort("data classes may not have lazy fields")
      val argFields = rawArgs.collect {
        case vd @ ValDef(mods, _, _, _) =>
          checkMods(mods)
          new SyntacticField(vd)
      }
      val bodyFields = rawStats.collect {
        case vd @ ValDef(mods, _, tpt, _) =>
          if (tpt.isEmpty)
            abort("Fields of data classes must have explicitly annotated types.",
                  at = vd.pos)
          checkMods(mods)
          new SyntacticField(vd)
      }
      tagField ++ argFields ++ bodyFields
    }
    val init = rawStats.collect {
      case t if t.isTerm               => t
      case ValDef(_, vname, tpt, value) =>
        q"$MethodModule.assigner[$name, $tpt]($ref, ${vname.toString}, $value)"
    }
    val methods = rawStats.collect { case t: DefDef => t }
    val types = rawStats.collect { case t: TypeDef => t }

    // Generate additional members
    val accessors = fields.flatMap { f =>
      val accessor = q"""
        def ${f.name}: ${f.tpt} =
          $MethodModule.accessor[$name, ${f.tpt}]($ref, ${f.name.toString})
      """
      val assignerName = TermName(f.name.toString + "_$eq")
      val assigner = q"""
        def $assignerName($value: ${f.tpt}): Unit =
          $MethodModule.assigner[$name, ${f.tpt}]($ref, ${f.name.toString}, $value)
      """
      if (!f.isMutable) accessor :: Nil
      else accessor :: assigner :: Nil
    }
    val argNames = fields.collect { case f if f.isCtorField => f.name }
    val _ns = argNames.zipWithIndex.map {
      case (argName, i) =>
        val _n = TermName("_" + (i + 1))
        q"def ${_n} = this.$argName"
    }
    val getBody = argNames match {
      case Nil          => q"this"
      case head :: Nil  => q"this.$head"
      case head :: tail => q"this"
    }
    val copyArgs = fields.collect { case f if f.isCtorField =>
      q"val ${f.name}: ${f.tpt} = this.${f.name}"
    }
    val initializer = if (init.isEmpty) q"" else q"def $initialize = { ..$init }"
    val args = fields.collect { case f if f.isCtorField =>
      q"val ${f.name}: ${f.tpt} = ${f.default}"
    }
    val unapplyTpt = if (argNames.isEmpty) tq"$BooleanClass" else tq"$name"
    val unapplyEmpty = if (argNames.isEmpty) q"false" else q"$termName.empty"

    val primaryExtractor = {
      val extractor = fresh("PrimaryExtractor")
      val body = if (argNames.isEmpty) q"true" else q"$scrutinee"
      q"""
        object $extractor {
          def unapply($scrutinee: $name): $unapplyTpt = $body
        }
      """
    }
    val parentExtractors = parents.map { p =>
      val extractor = fresh("ParentExtractor")
      val isC = q"$scrutinee.is[$name]"
      val asC = q"$scrutinee.as[$name]"
      val body =
        if (fields.filter(_.isCtorField).isEmpty) isC
        else q"if ($isC) $termName.${primaryExtractor.name}.unapply($asC) else $termName.empty"
      q"""
        object $extractor {
          def unapply($scrutinee: $p): $unapplyTpt = $body
        }
      """
    }
    val universalExtractor = {
      val extractor = fresh("UniversalExtractor")
      val parentCases = parents.zip(parentExtractors).map { case (p, u) =>
        cq"$scrutinee: $p => ${u.name}.unapply($scrutinee)"
      }
      q"""
        object $extractor {
          def unapply($scrutinee: $AnyClass): $unapplyTpt = $scrutinee match {
            case $scrutinee: $name => ${primaryExtractor.name}.unapply($scrutinee)
            case ..$parentCases
            case _                 => $unapplyEmpty
          }
        }
      """
    }
    val extractorAnnots =
      q"new $PrimaryExtractorClass($termName.${primaryExtractor.name})" ::
      q"new $UniversalExtractorClass($termName.${universalExtractor.name})" ::
      parents.zip(parentExtractors).map { case (p, u) =>
        q"new $ParentExtractorClass(new $TagClass[$p], $termName.${u.name})"
      }
    val mods = Modifiers(
      (rawMods.flags.asInstanceOf[Long] & Flag.FINAL.asInstanceOf[Long]).asInstanceOf[FlagSet],
      rawMods.privateWithin,
      q"new $DataClass" :: layout(fields) :: extractorAnnots ::: rawMods.annotations
    )

    q"""
      $mods class $name private (
        private val $ref: $RefClass
      ) extends $AnyValClass with ..$traits { $rawSelf =>
        import scala.language.experimental.{macros => $canUseMacros}

        ..$initializer
        ..$accessors

        def isEmpty  = $ref == null
        def nonEmpty = $ref != null
        def get      = $getBody
        ..${_ns}

        def copy(..$copyArgs)(implicit $memory: $MemoryClass): $name =
          $termName.apply(..$argNames)($memory)
        override def toString(): $StringClass =
          $MethodModule.toString[$name](this)

        def is[T]: $BooleanClass = macro $internal.macros.Method.is[$name, T]
        def as[T]: T             = macro $internal.macros.Method.as[$name, T]

        ..$types
        ..$methods
      }
      $companionMods object $termName
                     extends { ..$companionEarly }
                     with ..$companionParents { $companionSelf =>
        import scala.language.experimental.{macros => $canUseMacros}

        val empty: $name                       = null.asInstanceOf[$name]
        def fromRef($ref: $RefClass): $name    = new $name($ref)
        def toRef($instance: $name): $RefClass = $instance.$ref
        def apply(..$args)(implicit $memory: $MemoryClass): $name =
          $MethodModule.allocator[$name]($memory, ..$argNames)
        def unapply(scrutinee: $AnyClass): $unapplyTpt =
          macro $internal.macros.WhiteboxMethod.unapply[$name]

        $primaryExtractor
        $universalExtractor
        ..$parentExtractors

        ..$companionStats
      }
    """
  }

  def data(annottees: Tree*): Tree = annottees match {
    case (clazz: ClassDef) :: Nil =>
      dataTransform(clazz, q"object ${clazz.name.toTermName}")
    case (clazz: ClassDef) :: (companion: ModuleDef) :: Nil =>
      dataTransform(clazz, companion)
    case _ =>
      abort("@data anottation only works on classes")
  }

  def countClasses(stats: List[Tree]): Int = stats.map {
    case c: ClassDef  => 1
    case m: ModuleDef => countClasses(m.impl.body)
    case _            => 0
  }.sum

  def enumTransform(clazz: ClassDef, module: ModuleDef) = {
    val q"""
      $classMods class $name[..$classTargs] $classCtorMods(...$classArgss)
                 extends { ..$classEarly }
                 with ..$classParents { $classSelf => ..$classStats }
    """ = clazz
    val q"""
      $rawMods object $termName extends { ..$rawEarly }
               with ..$rawParents { $rawSelf => ..$rawStats }
    """ = module

    // Well-formedness checks
    if (classMods.flags != NoFlags)
      abort("enum classes may not have any modifiers")
    if (classTargs.nonEmpty)
      abort("enum classes may not have type arguments", at = classTargs.head.pos)
    if (classCtorMods.flags != NoFlags)
      abort("enum classes may not have constructor modifiers")
    if (classArgss != List(Nil))
      abort("enum classes may not have constructor arguments", at = classArgss.head.head.pos)
    if (classEarly.nonEmpty)
      abort("enum classes may not have early definitions", at = classEarly.head.pos)
    classParents match {
      case tq"$pkg.AnyRef" :: Nil if pkg.symbol == ScalaPackage =>
      case _ => abort("enum classes may not inherit from other classes", at = classParents.head.pos)
    }
    if (classStats.nonEmpty)
      abort("enum classes may not have body statements", at = classStats.head.pos)

    // Generate some fresh names
    val instance = fresh("instance")
    val coerce   = fresh("coerce")

    // Member and annotation transformation
    val groupedAnns = rawMods.annotations.groupBy {
      case q"new $ann[..$_](...$_)" =>
        ann.symbol match {
          case ParentClass        => 'parent
          case ClassTagRangeClass => 'range
          case _                  => 'rest
        }
    }
    val parents    = groupedAnns.get('parent).getOrElse(Nil)
    val rangeOpt   = groupedAnns.get('range).map(_.head)
    val moduleMods = rawMods.mapAnnotations { _ => groupedAnns.get('rest).getOrElse(Nil) }

    val total = countClasses(rawStats)
    def const(value: Int) =
      if (total < Byte.MaxValue)
        q"${value.toByte}: $ByteClass"
      else if (total < Short.MaxValue)
        q"${value.toShort}: $ShortClass"
      else
        q"$value: $IntClass"

    var count: Int = 0
    def parentAnnot =
      q"new $ParentClass(new $TagClass[$name]())"
    def classTagAnnot =
      q"new $ClassTagClass(${const(count)})"
    def classTagRangeAnnot(start: Int) =
      q"new $ClassTagRangeClass(${const(start)}, ${const(count)})"
    def transformStats(stats: List[Tree]): List[Tree] = stats.map {
      case c: ClassDef =>
        count += 1
        val mods = c.mods.mapAnnotations { anns =>
          if (parents.nonEmpty) parentAnnot :: anns
          else parentAnnot :: classTagAnnot :: anns
        }
        treeCopy.ClassDef(c, mods, c.name, c.tparams, c.impl)
      case m: ModuleDef =>
        val start = count
        val impl = treeCopy.Template(m.impl, m.impl.parents, m.impl.self,
                                     transformStats(m.impl.body))
        val mods = m.mods.mapAnnotations { anns =>
          if (parents.nonEmpty) parentAnnot :: anns
          else parentAnnot :: classTagRangeAnnot(start) :: anns
        }
        treeCopy.ModuleDef(m, mods, m.name, impl)
      case other =>
        other
    }
    val stats = transformStats(rawStats)

    val range =
      if (parents.nonEmpty) rangeOpt.get
      else q"new $ClassTagRangeClass(${const(0)}, ${const(count)})"
    val q"$_: $tagTpt" = const(0)
    val layt = layout(List(new SyntacticField(q"val $tag: $tagTpt")))
    val annots = q"new $EnumClass" :: layt :: range :: parents

    q"""
      @..$annots final class $name private(
        private val $ref: $RefClass
      ) extends $AnyValClass {
        import scala.language.experimental.{macros => $canUseMacros}
        def $tag: $tagTpt        = $MethodModule.accessor[$name, $tagTpt]($ref, ${tag.toString})
        def is[T]: $BooleanClass = macro $internal.macros.Method.is[$name, T]
        def as[T]: T             = macro $internal.macros.Method.as[$name, T]
      }
      $moduleMods object $termName extends { ..$rawEarly } with ..$rawParents { $rawSelf =>
        import scala.language.experimental.{macros => $canUseMacros}
        val empty: $name                       = null.asInstanceOf[$name]
        def fromRef($ref: $RefClass): $name    = new $name($ref)
        def toRef($instance: $name): $RefClass = $instance.$ref
        implicit def $coerce[T](t: T): $name   =
          macro $internal.macros.WhiteboxMethod.coerce[$name, T]
        ..$stats
      }
    """
  }

  def enum(annottees: Tree*): Tree = annottees match {
    case (clazz: ClassDef) :: Nil =>
      enumTransform(clazz, q"object ${clazz.name.toTermName}")
    case (clazz: ClassDef) :: (module: ModuleDef) :: Nil =>
      enumTransform(clazz, module)
    case (module: ModuleDef) :: Nil =>
      enumTransform(q"class ${module.name.toTypeName}", module)
    case _ =>
      abort("@enum anottation only works on objects and classes")
  }
}

class Region(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def open(pool: Tree) = q"???"

  def apply[T: WeakTypeTag](f: Tree)(pool: Tree) = {
    val r = freshVal("r", tpe = RegionClass.toType, value = open(pool))
    val res = fresh("res")
    val body = app(f, q"${r.symbol}")
    q"""
      $r
      val $res =
        try $body
        finally ${r.symbol}.close()
      $res
    """
  }
}

class Method(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  def throwNullRef = q"throw new _root_.java.lang.NullPointerException"

  def accessor[C: WeakTypeTag, T: WeakTypeTag](ref: Tree, name: Tree): Tree = {
    val C = wt[C]
    assertAllocatable(C)
    val ClassOf(fields, _, _) = C
    val q"${nameStr: String}" = name
    fields.collectFirst {
      case f if f.name.toString == nameStr =>
        val mem = q"$ref.memory"
        val tpes = fields.takeWhile(_ ne f).map(_.tpe)
        val offset = q"$mem.sizeOf[(..$tpes)]"
        read(q"$ref.addr + $offset", f.tpe, mem)
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
        val mem = q"$ref.memory"
        val tpes = fields.takeWhile(_ ne f).map(_.tpe)
        val offset = q"$mem.sizeOf[(..$tpes)]"
        write(q"$ref.addr + $offset", f.tpe, value, mem)
    }.getOrElse {
      abort(s"$C ($fields) doesn't have field `$nameStr`")
    }
  }

  // TODO: zero fields by default
  // TODO: zero-size data structures should not allocate any memory
  def allocator[C: WeakTypeTag](memory: Tree, args: Tree*): Tree = {
    val C = wt[C]
    val ClassOf(fields, _, tagOpt) = C
    val tagValueOpt = tagOpt.map { case (v, tpt) => v }
    val addr = fresh("addr")
    val size =
      if (fields.isEmpty) q"1"
      else {
        val fieldTpes = fields.map(_.tpe)
        q"$memory.sizeOf[(..$fieldTpes)]"
      }
    val writes = fields.zip(tagValueOpt ++: args).map { case (f, arg) =>
      val tpes = fields.takeWhile(_ ne f).map(_.tpe)
      val offset = q"$memory.sizeOf[(..$tpes)]"
      write(q"$addr + $offset", f.tpe, arg, memory)
    }
    val newC = q"new $C(new $RefClass($addr, $memory))"
    val instantiate = C.members.find(_.name == initialize).map { _ =>
      val instance = fresh("instance")
      q"""
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
    val path =
      (C :: parents.map(_.tpe))
        .reverse
        .map(_.typeSymbol.name.toString)
        .mkString("", ".", "")
    q"""
      val $sb = new $StringBuilderClass
      $sb.append($path)
      $sb.append("(")
      ..$appends
      $sb.append(")")
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

class WhiteboxMethod(val c: whitebox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }

  def unapply[C: WeakTypeTag](scrutinee: Tree): Tree = {
    val q"$_(..$args)" = c.macroApplication
    val C = wt[C]
    val T = scrutinee.tpe
    val extractor =
      if (C =:= T) {
        val ExtractPrimaryExtractor(q"new $_($_.${extractor: TermName})" :: Nil) = C.typeSymbol
        extractor
      } else if (isParent(T, C)) {
        val ExtractParentExtractor(extractors) = C.typeSymbol
        extractors.collectFirst {
          case q"new $_(new $_[$tpt](), $_.${extractor: TermName})"
            if tpt.tpe.typeSymbol == T.typeSymbol =>
            extractor
        }.get
      } else {
        val ExtractUniversalExtractor(q"new $_($_.${extractor: TermName})" :: Nil) = C.typeSymbol
        extractor
      }
    val companion = C.typeSymbol.companion
    q"$companion.$extractor.unapply(..$args)"
  }

  def coerce[C: WeakTypeTag, T: WeakTypeTag](t: Tree): Tree = {
    val T = wt[T]
    val C = wt[C]
    if (isParent(C, T)) cast(t, T, C)
    else abort(s"can't coerce $T to $C")
  }
}

class Memory(val c: blackbox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  object TupleOf {
    def unapply(tpe: Type): Option[List[Type]] =
      if (tpe.typeSymbol == UnitClass) Some(Nil)
      else TupleClass.seq.find(_ == tpe.typeSymbol).map(sym => tpe.baseType(sym).typeArgs)
  }

  def sizeOfSplit(tpe: Type): (Int, Int) = tpe match {
    case ByteTpe  | BooleanTpe => (1, 0)
    case ShortTpe | CharTpe    => (2, 0)
    case IntTpe   | FloatTpe   => (4, 0)
    case LongTpe  | DoubleTpe  => (8, 0)
    case ClassOf(_, _, _)      => (0, 1)
    case TupleOf(tpes)         =>
      tpes.map(sizeOfSplit).foldLeft((0, 0)) {
        case ((bytes1, refs1), (bytes2, refs2)) =>
          (bytes1 + bytes2, refs1 + refs2)
      }
    case _ =>
      abort(s"can't compute size of $tpe")
  }

  def sizeOf[T: WeakTypeTag] = {
    val (bytes, refs) = sizeOfSplit(weakTypeOf[T])
    if (refs == 0) q"$bytes"
    else q"$bytes + $refs * ${c.prefix}.sizeOfRef"
  }
}

class Array(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  lazy val A = c.prefix.tree.tpe.baseType(ArrayClass).typeArgs.head

  def isEmpty = q"${c.prefix.tree}.$ref == null"

  def nonEmpty = q"${c.prefix.tree}.$ref != null"

  def size = stabilized(c.prefix.tree) { pre =>
    read(q"$pre.$ref.addr", SizeTpe, q"$pre.$ref.memory")
  }

  def boundsChecked(index: Tree)(ifOk: Tree => Tree => Tree) =
    stabilized(c.prefix.tree) { pre =>
      stabilized(index) { idx =>
        val size = fresh("size")
        q"""
          val $size: $SizeTpe = ${read(q"$pre.$ref.addr", AddrTpe, q"$pre.$ref.memory")}
          if ($idx >= 0 && $idx < $size)  ${ifOk(pre)(idx)}
          else throw new _root_.java.lang.IndexOutOfBoundsException($index.toString)
        """
      }
    }

  def apply(index: Tree) =
    boundsChecked(index) { pre => i =>
      val addr = fresh("addr")
      val mem  = fresh("mem")
      q"""
        val $mem = $pre.$ref.memory
        val $addr = $pre.$ref.addr + $mem.sizeOf[$SizeTpe] + $i * $mem.sizeOf[$A]
        ${read(q"$addr", A, q"$mem")}
      """
    }

  def update(index: Tree, value: Tree) =
    boundsChecked(index) { pre => i =>
      val addr = fresh("addr")
      val mem  = fresh("mem")
      q"""
        val $mem = $pre.$ref.memory
        val $addr = $pre.$ref.addr + $mem.sizeOf[$SizeTpe] + $i * $mem.sizeOf[$A]
        ${write(q"$addr", A, value, q"$mem")}
      """
    }

  def foreach(f: Tree) =
    stabilized(c.prefix.tree) { pre =>
      val mem = fresh("mem")
      q"""
        if ($pre.$ref != null) {
          val $mem = $pre.$ref.memory
          ${iterate(A, pre, q"$mem", p => app(f, read(p, A, q"$mem")))}
        }
      """
    }

  def iterate(T: Type, pre: Tree, mem: Tree, f: Tree => Tree) = {
    val p = fresh("p")
    val len = fresh("len")
    val step = fresh("step")
    val bound = fresh("bound")
    q"""
      var $p: $AddrTpe = $pre.$ref.addr
      val $len: $SizeTpe = ${read(q"$p", SizeTpe, mem)}
      $p += $mem.sizeOf[$SizeTpe]
      val $step: $SizeTpe = $mem.sizeOf[$T]
      val $bound: $AddrTpe = $p + $len * $step
      while ($p < $bound) {
        ${f(q"$p")}
        $p += $step
      }
    """
  }

  def map[B: WeakTypeTag](f: Tree)(m: Tree) = {
    val B = wt[B]
    assertAllocatable(B)
    stabilized(c.prefix.tree) { pre =>
      stabilized(m) { mem =>
        val narr = fresh("narr")
        val v    = fresh("v")
        val p    = fresh("p")
        val step = fresh("step")
        q"""
          val $narr = $ArrayModule.uninit[$B]($pre.length)($mem)
          val $step = $mem.sizeOf[$B]
          var $p    = $narr.$ref.addr + $mem.sizeOf[$SizeTpe]
          $pre.foreach { $v: $A =>
            ${write(q"$p", B, app(f, q"$v"), mem)}
            $p += $step
          }
          $narr
        """
      }
    }
  }

  def uninit[T: WeakTypeTag](n: Tree)(m: Tree) = {
    val T = wt[T]
    assertAllocatable(T)
    stabilized(n) { len =>
      stabilized(m) { mem =>
        val addr = fresh("addr")
        q"""
          if ($len < 0) throw new $IllegalArgumentExceptionClass
          else if ($len == 0) $ArrayModule.empty[$T]
          else {
            val $addr = $mem.allocate($mem.sizeOf[$AddrTpe] + $len * $mem.sizeOf[$T])
            ${write(q"$addr", SizeTpe, len, mem)}
            $ArrayModule.fromRef[$T](new $RefClass($addr, $mem))
          }
        """
      }
    }
  }

  def vararg[T: WeakTypeTag](values: Tree*)(m: Tree) = {
    val T = wt[T]
    assertAllocatable(T, s"Can't allocate offheap array of $T")
    if (values.isEmpty)
      q"$ArrayModule.empty[$T]"
    else stabilized(m) { mem =>
      val arr    = fresh("arr")
      val addr   = fresh("adr")
      val tsize  = fresh("tsize")
      val writes = values.zipWithIndex.map { case (v, i) =>
        write(q"$addr + $i * $tsize", T, v, mem)
      }
      q"""
        val $arr = $ArrayModule.uninit[$T](${values.length})($mem)
        val $tsize = $mem.sizeOf[$T]
        val $addr = $arr.$ref.addr + $mem.sizeOf[$AddrTpe]
        ..$writes
        $arr
      """
    }
  }

  def fill[T: WeakTypeTag](n: Tree)(elem: Tree)(m: Tree) = debug("iterate") {
    stabilized(n) { len =>
      stabilized(m) { mem =>
        val T   = wt[T]
        val arr = fresh("arr")
        q"""
          val $arr = $ArrayModule.uninit[$T]($len)
          ${iterate(T, q"$arr", mem, p => write(p, T, elem, mem))}
          $arr
        """
      }
    }
  }

  def copy[T: WeakTypeTag](from: Tree, fromIndex: Tree, to: Tree, toIndex: Tree) = q"???"
}
