package offheap
package internal
package macros

import scala.reflect.macros.whitebox

class Annotations(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._
  import Flag._

  val Ref    = if (checked) tq"$RefClass"    else tq"$AddrTpe"

  def layout(fields: List[SyntacticField]): Tree = {
    val tuples = fields.map { f =>
      q"(${f.name.toString}, $PredefModule.classOf[${f.tpt}])"
    }
    q"new $LayoutAnnotationClass($LayoutModule.packed(..$tuples))"
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
        q"new $ParentExtractorClass($PredefModule.classOf[$p], $termName.${u.name})"
      }
    val uncheckedAnnot = if (checked) Nil else List(q"new $UncheckedClass")
    val mods = Modifiers(
      (rawMods.flags.asInstanceOf[Long] & Flag.FINAL.asInstanceOf[Long]).asInstanceOf[FlagSet],
      rawMods.privateWithin,
      q"new $DataClass" :: layout(fields) ::
      extractorAnnots ::: uncheckedAnnot ::: rawMods.annotations
    )

    q"""
      $mods class $name private (
        private val $ref: $Ref
      ) extends $AnyValClass with ..$traits { $rawSelf =>
        import scala.language.experimental.{macros => $canUseMacros}

        ..$initializer
        ..$accessors

        def isEmpty  = ${isNull(q"$ref")}
        def nonEmpty = ${notNull(q"$ref")}
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

        val empty: $name                  = null.asInstanceOf[$name]
        def fromRef($ref: $Ref): $name    = new $name($ref)
        def toRef($instance: $name): $Ref = $instance.$ref
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

    var count: Int = 0
    def parentAnnot =
      q"new $ParentClass($PredefModule.classOf[$name])"
    def classTagAnnot =
      q"new $ClassTagClass(${const(count)})"
    def classTagRangeAnnot(start: Int) =
      q"new $ClassTagRangeClass(${const(start)}, ${const(count)})"
    def transformStats(stats: List[Tree]): List[Tree] = stats.map {
      case c: ClassDef =>
        count += 1
        val mods = c.mods.mapAnnotations { anns =>
          if (parentAnnots.nonEmpty) parentAnnot :: anns
          else parentAnnot :: classTagAnnot :: anns
        }
        treeCopy.ClassDef(c, mods, c.name, c.tparams, c.impl)
      case m: ModuleDef =>
        val start = count
        val impl = treeCopy.Template(m.impl, m.impl.parents, m.impl.self,
                                     transformStats(m.impl.body))
        val mods = m.mods.mapAnnotations { anns =>
          if (parentAnnots.nonEmpty) parentAnnot :: anns
          else parentAnnot :: classTagRangeAnnot(start) :: anns
        }
        treeCopy.ModuleDef(m, mods, m.name, impl)
      case other =>
        other
    }
    val stats = transformStats(rawStats)

    val rangeAnnot =
      if (parentAnnots.nonEmpty) rangeAnnotOpt.get
      else q"new $ClassTagRangeClass(${const(0)}, ${const(count)})"
    val q"$_: $tagTpt" = const(0)
    val layoutAnnot    = layout(List(new SyntacticField(q"val $tag: $tagTpt")))
    val uncheckedAnnot = if (checked) Nil else List(q"new $UncheckedClass")
    val annots         = q"new $EnumClass" :: layoutAnnot :: rangeAnnot ::
                         (uncheckedAnnot ::: parentAnnots)

    q"""
      @..$annots final class $name private(
        private val $ref: $Ref
      ) extends $AnyValClass {
        import scala.language.experimental.{macros => $canUseMacros}
        def $tag: $tagTpt        = $MethodModule.accessor[$name, $tagTpt]($ref, ${tag.toString})
        def is[T]: $BooleanClass = macro $internal.macros.Method.is[$name, T]
        def as[T]: T             = macro $internal.macros.Method.as[$name, T]
      }
      $moduleMods object $termName extends { ..$rawEarly } with ..$rawParents { $rawSelf =>
        import scala.language.experimental.{macros => $canUseMacros}
        val empty: $name                     = null.asInstanceOf[$name]
        def fromRef($ref: $Ref): $name       = new $name($ref)
        def toRef($instance: $name): $Ref    = $instance.$ref
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
