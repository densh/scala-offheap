package offheap
package internal
package macros

import scala.reflect.macros.whitebox

class Annotations(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._
  import Flag._

  implicit class SyntacticField(vd: ValDef) {
    def mods      = vd.mods
    def name      = vd.name
    def tpt       = vd.tpt
    def default   = vd.rhs
    def isMutable = vd.mods.hasFlag(MUTABLE)
    def inCtor    = vd.mods.hasFlag(PARAMACCESSOR)
    def inBody    = !inCtor
  }

  // TODO: improve modifiers propagation and checking
  // TODO: hygienic reference to class type from companion?
  // TODO: validate that fields are not called _N
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
    if (rawArgs.length > 64)
      abort("data classes may not have more than 64 constructor arguments")
    rawArgs.headOption.foreach { arg =>
      if (arg.mods.hasFlag(IMPLICIT))
        abort("data classes may not have implicit arguments", at = arg.pos)
    }
    rawTraits.foreach {
      case q"${tq"$_[..$targs]"}(...$args)" =>
        if (args.nonEmpty || targs.nonEmpty)
          abort("data classes can only inherit from universal traits")
    }

    // Generate fresh names used in desugaring
    val alloc     = fresh("alloc")
    val instance  = fresh("instance")
    val scrutinee = fresh("scrutinee")
    val value     = fresh("value")

    // Process and existing members
    val termName = name.toTermName
    val traits = rawTraits match {
      case tq"$pkg.AnyRef" :: Nil if pkg.symbol == ScalaPackage => Nil
      case other                                                => other
    }
    val parents = rawMods.annotations.collect {
      case q"new $annot(${m: RefTree}.classOf[$tpt])" if annot.symbol == ParentClass =>
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
        q"$MethodModule.assign[$name, $tpt]($addr, ${vname.toString}, $value)"
    }
    val methods = rawStats.collect { case t: DefDef => t }
    val types = rawStats.collect { case t: TypeDef => t }

    // Generate additional members
    var prev = q""
    val accessors = fields.flatMap { f =>
      val ctorAnnot =
        if (f.inCtor) q"new $CtorClass"
        else q""
      val props =
        prev :: classOf(f.tpt) ::
        q"new $AnnotsClass(..$ctorAnnot, ..${f.mods.annotations})" :: Nil
      val annot: Tree =
        q"""
          new $FieldClass(${f.name.toString}, ..$props,
                          $LayoutModule.field[$name](..$props))
        """
      prev = q"this.${f.name}"
      val accessor = q"""
        @$annot def ${f.name}: ${f.tpt} =
          $MethodModule.access[$name, ${f.tpt}]($addr, ${f.name.toString})
      """
      val assignerName = TermName(f.name.toString + "_$eq")
      val assigner = q"""
        def $assignerName($value: ${f.tpt}): Unit =
          $MethodModule.assign[$name, ${f.tpt}]($addr, ${f.name.toString}, $value)
      """
      if (!f.isMutable) accessor :: Nil
      else accessor :: assigner :: Nil
    }
    val argNames = fields.collect { case f if f.inCtor => f.name }
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
    val copyArgs = fields.collect { case f if f.inCtor =>
      q"val ${f.name}: ${f.tpt} = this.${f.name}"
    }
    val initializr = if (init.isEmpty) q"" else q"def $initializer = { ..$init; this }"
    val applyArgs = fields.zipWithIndex.collect { case (f, i) if f.inCtor =>
      val name = TermName("_" + (i + 1))
      q"val $name: ${f.tpt} = ${f.default}"
    }
    val applyName = TermName("apply" + applyArgs.length)
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
        if (fields.filter(_.inCtor).isEmpty) isC
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
        q"new $ParentExtractorClass(${classOf(p)}, $termName.${u.name})"
      }
    val mods = Modifiers(
      (rawMods.flags.asInstanceOf[Long] & Flag.FINAL.asInstanceOf[Long]).asInstanceOf[FlagSet],
      rawMods.privateWithin,
      q"new $DataClass" ::
      extractorAnnots ::: rawMods.annotations
    )
    val completeAnnot = q"new $CompleteClass($LayoutModule.markComplete[$name])"

    q"""
      $mods class $name private (
        private val $addr: $AddrTpe
      ) extends $AnyValClass with ..$traits { $rawSelf =>
        import scala.language.experimental.{macros => $canUseMacros}

        ..$accessors
        ..$initializr

        @$completeAnnot
        def $complete: $UnitClass = ()

        def isEmpty  = ${isNull(q"$addr")}
        def nonEmpty = ${notNull(q"$addr")}
        def get      = $getBody
        ..${_ns}

        def copy(..$copyArgs)(implicit $alloc: $AllocatorClass): $name =
          $termName.apply(..$argNames)($alloc)
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
        def fromAddr($addr: $AddrTpe): $name   = new $name($addr)
        def toAddr($instance: $name): $AddrTpe = $instance.$addr
        def apply(..$applyArgs)(implicit alloc: $AllocatorClass): $name =
          macro $internal.macros.Allocate.$applyName[$name]
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
    val groupedAnnots = rawMods.annotations.groupBy {
      case q"new $ann[..$_](...$_)" =>
        ann.symbol match {
          case ParentClass        => 'parent
          case ClassTagRangeClass => 'range
          case _                  => 'rest
        }
    }
    val parentAnnots  = groupedAnnots.get('parent).getOrElse(Nil)
    val rangeAnnotOpt = groupedAnnots.get('range).map(_.head)
    val moduleMods    = rawMods.mapAnnotations { _ => groupedAnnots.get('rest).getOrElse(Nil) }

    val total = countClasses(rawStats)
    def const(value: Int) =
      if (total < Byte.MaxValue)
        q"${value.toByte}: $ByteClass"
      else if (total < Short.MaxValue)
        q"${value.toShort}: $ShortClass"
      else
        q"$value: $IntClass"

    var count = 0
    var children = List.empty[Tree]
    def parentAnnot =
      q"new $ParentClass(${classOf(tq"$name")})"
    def classTagAnnot =
      q"new $ClassTagClass(${const(count)})"
    def classTagRangeAnnot(start: Int) =
      q"new $ClassTagRangeClass(${const(start)}, ${const(count)})"
    def transformStats(pre: Tree, stats: List[Tree]): List[Tree] = stats.map {
      case c: ClassDef =>
        count += 1
        children ::= tq"$pre.${c.name}"
        val mods = c.mods.mapAnnotations { anns =>
          if (parentAnnots.nonEmpty) parentAnnot :: anns
          else parentAnnot :: classTagAnnot :: anns
        }
        treeCopy.ClassDef(c, mods, c.name, c.tparams, c.impl)
      case m: ModuleDef =>
        val start = count
        val impl = treeCopy.Template(m.impl, m.impl.parents, m.impl.self,
                                     transformStats(q"$pre.${m.name}", m.impl.body))
        val mods = m.mods.mapAnnotations { anns =>
          if (parentAnnots.nonEmpty) parentAnnot :: anns
          else parentAnnot :: classTagRangeAnnot(start) :: anns
        }
        treeCopy.ModuleDef(m, mods, m.name, impl)
      case other =>
        other
    }
    val stats = transformStats(q"$termName", rawStats)

    val childrenTypes  = children.map { c => q"$PredefModule.classOf[$c]" }
    val childrenAnnot  = q"new $PotentialChildrenClass(..$childrenTypes)"
    val rangeAnnot =
      if (parentAnnots.nonEmpty) rangeAnnotOpt.get
      else q"new $ClassTagRangeClass(${const(0)}, ${const(count)})"
    val q"$_: $tagTpt" = const(0)
    val annots         = q"new $EnumClass" :: rangeAnnot ::
                         childrenAnnot :: parentAnnots

    val tagprops = q"" :: classOf(tagTpt) :: q"new $AnnotsClass()" :: Nil

    q"""
      @..$annots final class $name private(
        private val $addr: $AddrTpe
      ) extends $AnyValClass {
        import scala.language.experimental.{macros => $canUseMacros}

        @$FieldClass(${tag.toString}, ..$tagprops, $LayoutModule.field[$name](..$tagprops))
        def $tag: $tagTpt         = $MethodModule.access[$name, $tagTpt]($addr, ${tag.toString})
        def is[T]: $BooleanClass  = macro $internal.macros.Method.is[$name, T]
        def as[T]: T              = macro $internal.macros.Method.as[$name, T]
      }
      $moduleMods object $termName extends { ..$rawEarly } with ..$rawParents { $rawSelf =>
        import scala.language.experimental.{macros => $canUseMacros}
        val empty: $name                       = null.asInstanceOf[$name]
        def fromAddr($addr: $AddrTpe): $name   = new $name($addr)
        def toAddr($instance: $name): $AddrTpe = $instance.$addr
        implicit def $coerce[T](t: T): $name =
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
