package offheap
package internal
package macros

import scala.collection.mutable
import scala.reflect.macros.{whitebox, blackbox}

trait Common {
  val c: blackbox.Context
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._
  import rootMirror.{staticClass, staticPackage}

  val OffheapClass       = staticClass("offheap.internal.annot.offheap")
  val RegionClass        = staticClass("offheap.Region")
  val StringBuilderClass = staticClass("scala.collection.mutable.StringBuilder")

  val offheap  = staticPackage("offheap")
  val internal = staticPackage("offheap.internal")
  val unsafe   = q"$internal.UnsafeMemory"
  val method   = q"$internal.Method"

  val tagName = TermName("$tag$")
  val tagSize = 4

  def abort(msg: String, at: Position = c.enclosingPosition): Nothing = c.abort(at, msg)

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

  case class Field(name: String, tpe: Type, offset: Long)

  class LayoutAnnotatedClass(val classSym: Symbol) {
    def is(sym: Symbol): Boolean =
      sym.annotations.exists(_.tpe.typeSymbol == classSym)
    def unapply(tpe: Type): Option[List[Field]] = unapply(tpe.widen.typeSymbol)
    def unapply(sym: Symbol): Option[List[Field]] = sym match {
      case sym: ClassSymbol if this.is(sym) =>
        val q"new $_($_(..$descriptors))" = sym.annotations.collectFirst {
          case ann if ann.tpe.typeSymbol == classSym => ann
        }.get.tree
        val buf = mutable.ListBuffer[Field]()
        var offset = 0
        descriptors.foreach { case q"(${name: String}, $_[$tpt]())" =>
          buf.append(Field(name, tpt.tpe, offset))
          offset += sizeof(tpt.tpe)
        }
        Some(buf.toList)
      case _ => None
    }
  }

  object ClassOf extends LayoutAnnotatedClass(OffheapClass)

  def read(tpe: Type, address: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val method = TermName(s"get$tpe")
      q"$unsafe.$method($address)"
    case BooleanTpe =>
      q"$unsafe.getByte($address) != ${Literal(Constant(0.toByte))}"
    case ClassOf(_) =>
      val companion = tpe.typeSymbol.companion
      q"$companion.fromAddr$$unsafe($unsafe.getLong($address))"
  }

  def write(tpe: Type, address: Tree, value: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val method = TermName(s"put$tpe")
      q"$unsafe.$method($address, $value)"
    case BooleanTpe =>
      q"""
        $unsafe.putByte($address,
                        if ($value) ${Literal(Constant(1.toByte))}
                        else ${Literal(Constant(0.toByte))})
      """
    case ClassOf(_) =>
      val companion = tpe.typeSymbol.companion
      q"$unsafe.putLong($address, $companion.toAddr$$unsafe($value))"
  }

  def sizeof(tpe: Type): Int = tpe match {
    case ByteTpe  | BooleanTpe             => 1
    case ShortTpe | CharTpe                => 2
    case IntTpe   | FloatTpe               => 4
    case LongTpe  | DoubleTpe              => 8
    case tpe if ClassOf.is(tpe.typeSymbol) => 8
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

  def layout(fields: List[OField]): Tree = {
    val tuples = fields.map { f =>
      q"(${f.name.toString}, $internal.annot.Tag[${f.tpt}]())"
    }
    q"$internal.annot.Layout((${tagName.toString}, $internal.annot.Tag[$IntClass]), ..$tuples)"
  }

  case class OField(name: TermName, tpt: Tree, default: Tree,
                    isMutable: Boolean, isCtorField: Boolean)

  // TODO: handle generics
  // TODO: handle implicit parameters
  // TODO: handle existing companions
  // TODO: modifiers propagation
  // TODO: hygienic reference to class type from companion?
  def offheapAnnotation(annottees: Tree*): Tree = debug("offheap")(annottees match {
    case q"""
        $rawMods class $name[..$rawTargs](..$rawArgs) extends ..$rawParents { ..$rawStats }
      """ :: Nil =>
      // Process and validate existing members
      val fields = {
        def checkMods(mods: Modifiers) =
          if (mods.hasFlag(LAZY)) abort("lazy vals are not supported")
        val argFields = rawArgs.collect {
          case ValDef(mods, name, tpt, default) =>
            checkMods(mods)
            OField(name, tpt, default,
                   isMutable = mods.hasFlag(MUTABLE),
                   isCtorField = true)
        }
        val bodyFields = rawStats.collect {
          case ValDef(mods, name, tpt, _) =>
            checkMods(mods)
            OField(name, tpt, EmptyTree,
                   isMutable = mods.hasFlag(MUTABLE),
                   isCtorField = false)
        }
        argFields ++ bodyFields
      }
      val init = rawStats.collect {
        case t if t.isTerm             => t
        case ValDef(_, name, _, value) => ???
      }
      val traits = rawParents.map {
        case q"${tq"$ref[..$targs]"}(...$args)" =>
          if (args.nonEmpty || targs.nonEmpty)
            abort("offheap classes can only inherit from offheap traits")
          ref
      }
      val methods = rawStats.collect { case t: DefDef => t }
      val types = rawStats.collect { case t: TypeDef => t }
      // Generate additional members
      val tagField = OField(tagName, tq"$IntClass", EmptyTree, false, false)
      val addrName = TermName("$addr$")
      val addr = q"this.$addrName"
      val accessors = (tagField +: fields).flatMap { f =>
        val accessor = q"""
          def ${f.name}: ${f.tpt} =
            $method.accessor[$name, ${f.tpt}]($addr, ${f.name.toString})
        """
        val assignerName = TermName(f.name.toString + "_$eq")
        val value = fresh("value")
        val assigner = q"""
          def $assignerName($value: ${f.tpt}): Unit =
            $method.assigner[$name, ${f.tpt}]($addr, ${f.name.toString}, $value)
        """
        if (!f.isMutable) accessor :: Nil
        else accessor :: assigner :: Nil
      }
      val argNames = fields.collect { case f if f.isCtorField => f.name }
      val r = fresh("r")
      val instance = fresh("instance")
      val address = fresh("address")
      val scrutinee = fresh("scrutinee")
      val caseClassSupport =
        if (!rawMods.hasFlag(Flag.CASE)) q""
        else {
          val _ns = argNames.zipWithIndex.map {
            case (argName, i) =>
              val _n = TermName("_" + (i + 1))
              q"def ${_n} = this.$argName"
          }
          val copyArgs = fields.collect { case f if f.isCtorField =>
            q"val ${f.name}: ${f.tpt} = this.${f.name}"
          }
          q"""
            def isEmpty  = $addr == 0
            def nonEmpty = $addr != 0
            def get      = this
            ..${_ns}
            def copy(..$copyArgs)(implicit $r: $RegionClass): $name =
              $method.copy[$name]($r, ..$argNames)
            override def toString(): $StringClass =
              $method.toString[$name]
          """
        }
      // Wrap everything into a nice shiny package
      val args = fields.collect { case f if f.isCtorField =>
        q"val ${f.name}: ${f.tpt} = ${f.default}"
      }
      q"""
        @$internal.annot.offheap(${layout(fields)}) final class $name private(
          private val $addrName: $LongClass
        ) extends $AnyValClass {
          def $$initialize$$ = { ..$init }
          ..$accessors
          ..$methods
          ..$types
          ..$caseClassSupport
        }
        object ${name.toTermName} {
          val $tagName = $internal.Tag.next
          def apply(..$args)(implicit $r: $RegionClass): $name =
            $method.allocator[$name]($r, ..$argNames)
          def unapply($scrutinee: $name): $name = $scrutinee
          val empty: $name = null.asInstanceOf[$name]
          def fromAddr$$unsafe($address: $LongClass): $name =
            new $name($address)
          def toAddr$$unsafe($instance: $name): $LongClass =
            $instance.$addrName
        }
      """
  })
}

class Region(val c: blackbox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def apply[T: WeakTypeTag](f: Tree) = {
    val r = freshVal("r", tpe = RegionClass.toType,
                     value = q"$offheap.Region.open()")
    val res = fresh("res")
    q"""
      $r
      val $res =
        try ${app(f, q"${r.symbol}")}
        finally ${r.symbol}.close()
      $res
    """
  }
}

class Method(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  def throwNullRef = q"throw $offheap.NullRefException"

  def accessor[C: WeakTypeTag, T: WeakTypeTag](addr: Tree, name: Tree): Tree = {
    val C = wt[C]
    val ClassOf(fields) = C
    ensureAllocatable(wt[C])
    val q"${nameStr: String}" = name
    fields.collectFirst {
      case f if f.name.toString == nameStr =>
        val r = read(f.tpe, q"$addr + ${f.offset}")
        q"""
          if ($addr == 0) $throwNullRef
          else $r
        """
    }.getOrElse {
      abort(s"$C ($fields) doesn't have field `$nameStr`")
    }
  }

  def assigner[C: WeakTypeTag, T: WeakTypeTag](addr: Tree, name: Tree, value: Tree) = {
    val C = wt[C]
    val ClassOf(fields) = C
    ensureAllocatable(wt[C])
    val q"${nameStr: String}" = name
    fields.collectFirst {
      case f if f.name.toString == nameStr =>
        val r = write(f.tpe, q"$addr + ${f.offset}", value)
        q"""
          if ($addr == 0) $throwNullRef
          else $r
        """
    }.getOrElse {
      abort(s"$C ($fields) doesn't have field `$nameStr`")
    }
  }

  def allocator[C: WeakTypeTag](r: Tree, args: Tree*): Tree = {
    val C = wt[C]
    val companion = C.typeSymbol.companion
    val ClassOf(fields) = C.typeSymbol
    val size = fields.map(f => sizeof(f.tpe)).sum
    val addr = fresh("addr")
    val writes = fields.zip(q"$companion.$tagName" +: args).map { case (f, arg) =>
      write(f.tpe, q"$addr + ${f.offset}", arg)
    }
    q"""
      val $addr = $r.allocate($size)
      ..$writes
      new $C($addr)
    """
  }

  def copy[C](r: Tree, args: Tree*): Tree = q"???"

  def toString[C]: Tree = q"???"
}
