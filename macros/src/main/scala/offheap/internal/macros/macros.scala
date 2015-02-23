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
  val RefClass           = staticClass("offheap.Ref")
  val RegionClass        = staticClass("offheap.Region")
  val StringBuilderClass = staticClass("scala.collection.mutable.StringBuilder")

  val offheap  = staticPackage("offheap")
  val internal = staticPackage("offheap.internal")
  val unsafe   = q"$internal.Unsafer.unsafe"

  val tag     = TermName("$tag$")
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
      case sym if sym == RefClass                                  => true
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

  def layout(args: List[Tree]): Tree = {
    val tuples = args.map { case q"$_ val $name: $tpt = $_" =>
      q"(${name.toString}, $internal.annot.Tag[$tpt]())"
    }
    q"$internal.annot.Layout((${tag.toString}, $internal.annot.Tag[$IntClass]), ..$tuples)"
  }

  // TODO: handle default arguments
  // TODO: handle parents
  // TODO: handle generics
  // TODO: handle implicit parameters
  // TODO: handle non-term/non-def stats
  // TODO: handle existing companions
  // TODO: modifiers propagation
  // TODO: hygienic reference to class type from companion?
  def offheapAnnotation(annottees: Tree*): Tree = annottees match {
    case q"$cmods class $name(..$args) extends ..$parents { ..$stats }" :: Nil =>
      if (args.isEmpty)
        abort("offheap classes require at least one parameter")
      val addrName = TermName("$addr$")
      val addr = q"this.$addrName"
      val accessors = (q"val $tag: $IntClass" +: args).flatMap {
        case q"$_ var $argName: $tpt = $_" =>
          val value = fresh("value")
          val assignerName = TermName(argName.toString + "_=")
          val q"..$stats" = q"""
            def $argName: $tpt =
              $internal.Method.accessor[$name, $tpt]($addr, ${argName.toString})

            def $assignerName($value: $tpt): Unit =
              $internal.Method.assigner[$name, $tpt]($addr, ${argName.toString}, $value)
          """
          stats
        case q"$_ val $argName: $tpt = $_" =>
          q"""
            def $argName: $tpt =
              $internal.Method.accessor[$name, $tpt]($addr, ${argName.toString})
          """ :: Nil
      }
      val methods: List[Tree] = stats.collect {
        case q"$_ def $methodName[..$targs](...$argss): $tpt = $body" =>
          q"""
            def $methodName[..$targs](...$argss): $tpt =
              $internal.Method.method($body)
          """
      }
      val init = stats.filter { _.isTerm }
      val argNames = args.map { case q"$_ val $name: $_ = $_" => name }
      val _ns = args.zipWithIndex.map {
        case (q"$_ val $argName: $_ = $_", i) =>
          val _n = TermName("_" + (i + 1))
          q"def ${_n} = this.$argName"
      }
      val r = fresh("r")
      val instance = fresh("instance")
      val address = fresh("address")
      val scrutinee = fresh("scrutinee")
      val copyArgs = args.map { case q"$_ val $name: $tpt = $_" =>
        q"val $name: $tpt = this.$name"
      }
      val caseClassSupport =
        if (!cmods.hasFlag(Flag.CASE)) q""
        else q"""
          def isEmpty  = $addr == 0
          def nonEmpty = $addr != 0
          def get      = this
          ..${_ns}
          def copy(..$copyArgs)(implicit $r: $RegionClass): $name =
            $internal.Method.copy[$name]($r, ..$argNames)
          override def toString(): $StringClass =
            $internal.Method.toString[$name]
        """
      q"""
        @$internal.annot.offheap(${layout(args)}) final class $name private(
          private val $addrName: $LongClass
        ) extends $AnyValClass {
          def $$initialize$$ = { ..$init }
          ..$accessors
          ..$methods
          ..$caseClassSupport
        }
        object ${name.toTermName} {
          val $tag = $internal.Tag.next
          def apply(..$args)(implicit $r: $RegionClass): $name =
            $internal.Method.allocator[$name]($r, ..$argNames)
          def unapply($scrutinee: $name): $name = $scrutinee
          val empty: $name = null.asInstanceOf[$name]
          def fromAddr$$unsafe($address: $LongClass): $name =
            new $name($address)
          def toAddr$$unsafe($instance: $name): $LongClass =
            $instance.$addrName
        }
      """
  }
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
          $r
        """
    }.getOrElse {
      abort(s"$C ($fields) doesn't have field `$nameStr`")
    }
  }

  def assigner[C, T](addr: Tree,  name: Tree, value: Tree) = q"???"

  def allocator[C: WeakTypeTag](r: Tree, args: Tree*): Tree = {
    val C = wt[C]
    val companion = C.typeSymbol.companion
    val ClassOf(fields) = C.typeSymbol
    val size = fields.map(f => sizeof(f.tpe)).sum
    val addr = fresh("addr")
    val writes = fields.zip(q"$companion.$tag" +: args).map { case (f, arg) =>
      write(f.tpe, q"$addr + ${f.offset}", arg)
    }
    q"""
      val $addr = $r.allocate($size)
      ..$writes
      new $C($addr)
    """
  }

  def method[T](body: Tree): Tree = body

  def copy[C](r: Tree, args: Tree*): Tree = q"???"

  def toString[C]: Tree = q"???"
}
