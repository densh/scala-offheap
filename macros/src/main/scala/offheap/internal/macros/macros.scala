package offheap
package internal
package macros

import scala.collection.mutable
import scala.reflect.macros.{whitebox, blackbox}

trait Definitions {
  val c: blackbox.Context
  val bitDepth: Int = 64

  import c.universe._
  import c.universe.definitions._
  import c.universe.rootMirror._

  private val prefix = s"offheap.x$bitDepth"

  val MethodModule  = staticModule("offheap.internal.Method")
  val PoolModule    = staticModule(s"$prefix.Pool")

  val StringBuilderClass = staticClass("scala.collection.mutable.StringBuilder")
  val RegionClass        = staticClass(s"$prefix.Region")
  val RefClass           = staticClass(s"$prefix.Ref")
  val MemoryClass        = staticClass(s"$prefix.Memory")
  val OffheapClass       = staticClass("offheap.internal.annot.offheap")
  val TagClass           = staticClass("offheap.internal.annot.Tag")
  val LayoutClass        = staticClass("offheap.internal.annot.Layout")

  val internal = staticPackage("offheap.internal")

  val initialize = TermName("$initialize")

  val tag = TermName("$tag")
  val Tag = ShortClass
}

trait Common extends Definitions {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  def abort(msg: String, at: Position = c.enclosingPosition): Nothing = c.abort(at, msg)

  def panic(msg: String): Nothing = abort(s"panic: $msg")

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

  object Primitive {
    def unapply(tpe: Type): Boolean = tpe.typeSymbol match {
      case sym: ClassSymbol if sym.isPrimitive && sym != UnitClass => true
      case _                                                       => false
    }
  }

  object Allocatable {
    def unapply(tpe: Type): Boolean = tpe match {
      case Primitive() | ClassOf(_) => true
      case _                        => false
    }
  }

  case class Field(name: String, tpe: Type)

  class LayoutAnnotatedClass(val classSym: Symbol) {
    def is(sym: Symbol): Boolean =
      sym.annotations.exists(_.tpe.typeSymbol == classSym)
    def unapply(tpe: Type): Option[List[Field]] = unapply(tpe.widen.typeSymbol)
    def unapply(sym: Symbol): Option[List[Field]] = sym match {
      case sym: ClassSymbol if this.is(sym) =>
        sym.annotations.collectFirst {
          case ann if ann.tpe.typeSymbol == classSym => ann.tree
        }.map {
          case q"new $_(new $_(..$descriptors))" =>
            descriptors.map { case q"(${name: String}, new $_[$tpt]())" =>
              Field(name, tpt.tpe)
            }
          case q"new $_(new $_)" =>
            Nil
          case other =>
            panic(s"can't parse annotation $other")
        }
      case _ => None
    }
  }

  object ClassOf extends LayoutAnnotatedClass(OffheapClass)

  def read(addr: Tree, tpe: Type, memory: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val getT = TermName(s"get$tpe")
      q"$memory.$getT($addr)"
    case BooleanTpe =>
      q"$memory.getByte($addr) != ${Literal(Constant(0.toByte))}"
    case ClassOf(_) =>
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
    case ClassOf(_) =>
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

  def ensureAllocatable(T: Type): Unit =  T match {
    case Allocatable() => ()
    case _             => abort(s"$T is not fixed sized allocatable object")
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
  // TODO: gracefully fail on inner classes and objects
  def offheapTransform(clazz: Tree, companion: Tree) = {
    // Parse the input trees
    val q"""
      $rawMods class $name[..$rawTargs] $rawCtorMods(..$rawArgs)
                                                    (...$rawRestArgs)
               extends { ..$rawEarly }
               with ..$rawParents { $rawSelf => ..$rawStats }
    """ = clazz
    val q"""
      $companionMods object $_
                     extends { ..$companionEarly }
                     with ..$companionParents { $companionSelf => ..$companionStats }
    """ = companion

    // Checks that are shared between abstract and concrete transforms
    if (rawTargs.nonEmpty)
      abort("offheap classes don't support generics", at = rawTargs.head.pos)
    if (rawEarly.nonEmpty)
      abort("offheap classes don't suport early initializer", at = rawEarly.head.pos)
    if (rawMods.hasFlag(IMPLICIT))
      abort("implicit offheap classes are not supported")
    if (rawMods.hasFlag(CASE))
      abort("offheap classes are case-like by default")

    // Generate fresh names used in desugaring
    val ref          = fresh("ref")
    val memory       = fresh("memory")
    val instance     = fresh("instance")
    val scrutinee    = fresh("scrutinee")
    val value        = fresh("value")
    val canUseMacros = fresh("canUseMacros")

    def transformConcrete = {
      // Process and validate existing members
      if (rawRestArgs.nonEmpty)
        abort("offheap classes may not have more than one argument list",
              at = rawRestArgs.head.head.pos)
      rawArgs.headOption.foreach { arg =>
        if (arg.mods.hasFlag(IMPLICIT))
          abort("offheap classes may not have implicit arguments", at = arg.pos)
      }
      val fields = {
        def checkMods(mods: Modifiers) =
          if (mods.hasFlag(LAZY))
            abort("offheap classes may not have lazy fields")
        val argFields = rawArgs.collect {
          case vd @ ValDef(mods, _, _, _) =>
            checkMods(mods)
            new SyntacticField(vd)
        }
        val bodyFields = rawStats.collect {
          case vd @ ValDef(mods, _, tpt, _) =>
            if (tpt.isEmpty)
              abort("Fields of offheap classes must have explicitly annotated types.",
                    at = vd.pos)
            checkMods(mods)
            new SyntacticField(vd)
        }
        argFields ++ bodyFields
      }
      val init = rawStats.collect {
        case t if t.isTerm               => t
        case ValDef(_, vname, tpt, value) =>
          q"$MethodModule.assigner[$name, $tpt]($ref, ${vname.toString}, $value)"
      }
      val parents = rawParents.map {
        case q"${tq"$ref[..$targs]"}(...$args)" =>
          if (args.nonEmpty || targs.nonEmpty)
            abort("offheap classes can only inherit from " +
                  "abstract offheap classes and universal traits")
          ref
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
      val copyArgs = fields.collect { case f if f.isCtorField =>
        q"val ${f.name}: ${f.tpt} = this.${f.name}"
      }
      val initializer = if (init.isEmpty) q"" else q"def $initialize = { ..$init }"
      val args = fields.collect { case f if f.isCtorField =>
        q"val ${f.name}: ${f.tpt} = ${f.default}"
      }
      val body = q"""
        ..$types
        ..$initializer
        ..$accessors
        ..$methods

        def isEmpty  = $ref == null
        def nonEmpty = $ref != null
        def get      = this
        ..${_ns}

        def copy(..$copyArgs)(implicit $memory: $MemoryClass): $name =
          ${name.toTermName}.apply(..$argNames)($memory)
        override def toString(): $StringClass =
          $MethodModule.toString[$name](this)

        import scala.language.experimental.{macros => $canUseMacros}
        def is[T]: $BooleanClass =
          macro $internal.macros.Method.is[$name, T]
        def as[T]: T =
          macro $internal.macros.Method.as[$name, T]
      """
      val companionBody = q"""
        def apply(..$args)(implicit $memory: $MemoryClass): $name =
          $MethodModule.allocator[$name]($memory, ..$argNames)

        def unapply($scrutinee: $name): $name = $scrutinee
      """
      (body, companionBody, fields)
    }

    def transformAbstract = {
      if (rawArgs.nonEmpty || rawRestArgs.nonEmpty)
        abort("abstract offheap classes may not have constructor arguments")
      val methods = rawStats.map {
        case dd: DefDef =>
          if (dd.rhs.isEmpty)
            abort("offheap abstract classes may not contain abstract methods",
                  at = dd.pos)
        case other =>
          abort("offheap abstract classes can only contain concrete methods",
                at = other.pos)
      }
      val body: Tree = q"""
        ..$methods

        def $tag: $Tag =
          $MethodModule.accessor[$name, $Tag]($ref, ${tag.toString})

        def is[T]: $BooleanClass =
          macro $internal.macros.Method.is[$name, T]

        def as[T]: T =
          macro $internal.macros.Method.as[$name, T]
      """
      val fields = List(new SyntacticField(q"val $tag: $Tag"))
      (body, q"", fields)
    }

    val (body, companionBody, fields) =
      if (rawMods.hasFlag(ABSTRACT))
        transformAbstract
      else
        transformConcrete

    val annot = q"new $OffheapClass(${layout(fields)})"

    q"""
      @$annot final class $name private(
        private val $ref: $RefClass
      ) extends $AnyValClass { $rawSelf =>
        ..$body
      }
      object ${name.toTermName}
          extends { ..$companionEarly }
          with ..$companionParents { $companionSelf =>
        ..$companionStats
        ..$companionBody
        val empty: $name = null.asInstanceOf[$name]
        def fromRef($ref: $RefClass): $name =
          new $name($ref)
        def toRef($instance: $name): $RefClass =
          $instance.$ref
      }
    """
  }

  def offheapAnnotation(annottees: Tree*): Tree = annottees match {
    case (clazz: ClassDef) :: Nil =>
      offheapTransform(clazz, q"object ${clazz.name.toTermName}")
    case (clazz: ClassDef) :: (companion: ModuleDef) :: Nil =>
      offheapTransform(clazz, companion)
    case _ =>
      abort("@offheap anottation only works on classes")
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
    val ClassOf(fields) = C
    ensureAllocatable(wt[C])
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
    val ClassOf(fields) = C
    ensureAllocatable(wt[C])
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

  // TODO: setMemory(addr, size, 0) ?
  def allocator[C: WeakTypeTag](memory: Tree, args: Tree*): Tree = {
    val C = wt[C]
    val ClassOf(fields) = C
    val addr = fresh("addr")
    val size =
      if (fields.isEmpty) q"1"
      else {
        val fieldTpes = fields.map(_.tpe)
        q"$memory.sizeOf[(..$fieldTpes)]"
      }
    val writes = fields.zip(args).map { case (f, arg) =>
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
    val ClassOf(fields) = C
    val sb = fresh("sb")
    val appends =
      if (fields.isEmpty) Nil
      else fields.flatMap { f =>
        List(q"$sb.append($self.${TermName(f.name)})", q"""$sb.append(", ")""")
      }.init
    q"""
      val $sb = new $StringBuilderClass
      $sb.append(${C.typeSymbol.name.toString})
      $sb.append("(")
      ..$appends
      $sb.append(")")
      $sb.toString
    """
  }

  def is[C: WeakTypeTag, T: WeakTypeTag]: Tree = q"???"

  def as[C: WeakTypeTag, T: WeakTypeTag]: Tree = q"???"
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
    case ByteTpe  | BooleanTpe             => (1, 0)
    case ShortTpe | CharTpe                => (2, 0)
    case IntTpe   | FloatTpe               => (4, 0)
    case LongTpe  | DoubleTpe              => (8, 0)
    case tpe if ClassOf.is(tpe.typeSymbol) => (0, 1)
    case TupleOf(tpes)                     =>
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
