package regions.internal.macros

import scala.collection.mutable
import scala.reflect.macros.whitebox.Context

trait Common {
  val c: Context
  import c.universe._
  import c.universe.definitions._

  val StructClass = rootMirror.staticClass("regions.internal.runtime.struct")
  val UnionClass  = rootMirror.staticClass("regions.internal.runtime.union")
  val RefClass    = rootMirror.staticClass("regions.Ref")
  // val FatRefClass = rootMirror.staticClass("regions.FatRef")

  val regions  = q"_root_.regions"
  val internal = q"$regions.internal"
  val runtime  = q"$internal.runtime"
  val unsafe   = q"$runtime.unsafe"

  def abort(msg: String, at: Position = c.enclosingPosition): Nothing = c.abort(at, msg)
  def wt[T: WeakTypeTag]: Type        = weakTypeOf[T]
  def fresh(pre: String): TermName    = TermName(c.freshName(pre))

  def debug[T](header: String)(f: => T): T = {
    val res = f
    println(s"$header = $res")
    res
  }

  object RefOf {
    def unapply(tpe: Type): Option[(Type, Type)] =
      if (tpe.typeSymbol != RefClass) None
      else {
        val base = tpe.baseType(RefClass)
        Some((base.typeArgs(0), base.typeArgs(1)))
      }
  }

  object ArrayOf {
    def unapply(tpe: Type): Option[Type] =
      if (tpe.typeSymbol == ArrayClass) Some(tpe.typeArgs.head)
      else None
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
      case Primitive() | RefOf(_, _) | StructOf(_) => true
      case _                                       => false
    }
  }

  case class StructField(name: TermName, tpe: Type, offset: Long)

  object StructOf {
    def unapply(tpe: Type): Option[List[StructField]] = tpe.typeSymbol match {
      case sym: ClassSymbol if sym.annotations.exists(_.tpe.typeSymbol == StructClass) =>
        val args = tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.head.map { arg => (arg.name.toTermName, arg.info) }
        val buf = mutable.ListBuffer[StructField]()
        var offset = 0
        args.foreach { case (name, tpe) =>
          buf.append(StructField(name, tpe, offset))
          offset += sizeof(tpe)
        }
        Some(buf.toList)
      case _ => None
    }
  }

  def read(tpe: Type, address: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val method = TermName(s"get$tpe")
      q"$unsafe.$method($address)"
    case BooleanTpe =>
      q"$unsafe.getByte($address) != ${Literal(Constant(0.toByte))}"
    case RefOf(targ, r) =>
      val value = read(LongTpe, address)
      q"new $regions.Ref[$targ, $r]($value)"
    case StructOf(fields) =>
      val companion = tpe.typeSymbol.companion
      val tmps = fields.map { f => fresh(f.name.decoded) }
      val reads = fields.zip(tmps).map { case (f, tmp) =>
        val rhs = read(f.tpe, q"$address + ${f.offset}")
        q"val $tmp = $rhs"
      }
      q"""
        ..$reads
        $companion.apply(..$tmps)
      """
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
    case RefOf(_, _) =>
      write(LongTpe, address, q"$value.addr")
    case StructOf(fields) =>
      val v = fresh("v")
      val writes = fields.map { f =>
        write(f.tpe, q"$address + ${f.offset}", q"$v.${f.name}")
      }
      q"val $v = $value; ..$writes"
  }

  def sizeof(tpe: Type): Int = tpe match {
    case ByteTpe  | BooleanTpe              => 1
    case ShortTpe | CharTpe                 => 2
    case IntTpe   | FloatTpe                => 4
    case LongTpe  | DoubleTpe | RefOf(_, _) => 8
    case StructOf(fields)                   => fields.map(f => sizeof(f.tpe)).sum
  }

  def app(f: Tree, argValue: Tree) = f match {
    case q"($_ => $_)" =>
      import c.internal._, c.internal.decorators._
      val q"($param => $body)" = f
      val q"$_ val $_: $argTpt = $_" = param
      val arg = fresh("arg")
      val argSym = enclosingOwner.newTermSymbol(arg).setInfo(argTpt.tpe)
      val argDef = valDef(argSym, argValue)
      changeOwner(body, f.symbol, enclosingOwner)
      val transformedBody = typingTransform(body) { (tree, api) =>
        tree match {
          case id: Ident if id.symbol == param.symbol =>
            api.typecheck(q"$argSym")
          case _ =>
            api.default(tree)
        }
      }
      q"$argDef; $transformedBody"
    case _             =>
      q"$f($argValue)"
  }

  def paramTpe(f: Tree) = f.tpe.typeArgs.head
}

class Annotations(val c: Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def struct(annottees: Tree*): Tree = annottees match {
    case q"class $name(..$args)" :: Nil =>
      if (args.isEmpty) abort("structs require at least one argument")
      val checks = args.map {
        case q"$_ val $name: $tpt = $default" =>
          if (default.nonEmpty) abort("structs with default values are not supported")
          q"$internal.Ensure.allocatable[$tpt]"
      }
      val nargs = args.map { case q"$_ val $name: $tpt = $_" => q"val $name: $tpt" }
      q"""
        @_root_.regions.internal.struct case class $name(..$nargs) {
          ..$checks
        }
      """
  }

  def union(annottees: Tree*): Tree = ???
}

class Ensure(val c: Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def allocatable[T: WeakTypeTag]: Tree = {
    val T = wt[T]
    T match {
      case Allocatable() => q""
      case _             => abort(s"$T is not fixed sized allocatable object")
    }
  }
}

class Ref(val c: Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  lazy val A   = RefOf.unapply(c.prefix.tree.tpe).get._1
  lazy val R   = RefOf.unapply(c.prefix.tree.tpe).get._2
  lazy val pre = fresh("pre")

  def stabilized(value: Tree)(f: TermName => Tree) = {
    val stable = fresh("stable")
    q"val $stable = $value; ${f(stable)}"
  }

  def stabilizedPrefix(body: Tree) =
    q"val $pre = ${c.prefix}; $body"

  def branchEmpty(nonEmpty: Tree, empty: Tree) =
    stabilizedPrefix(q"if ($pre.addr != 0) $nonEmpty else $empty")

  def readValue = read(A, q"$pre.addr")

  def writeValue(value: Tree) = write(A, q"$pre.addr", value)

  def emptyRef(T: Type, R: Type) =
    q"null.asInstanceOf[$RefClass[$T, $R]]"

  def allocRef(T: Type, value: Tree, r: Tree) = {
    val v = fresh("v")
    val ref = fresh("ref")
    val size = T match {
      case Allocatable() => sizeof(T)
      case _             => abort(s"allocation of $T is not supported")
    }
    stabilized(value) { v =>
      q"""
        val $ref = new $regions.Ref[$T, ${r.symbol}.type]($runtime.allocMemory($r, $size))
        $ref.set($v)
        $ref
      """
    }
  }

  def throwEmptyRef =
    q"throw $regions.EmptyRefException"

  def isEmpty: Tree =
    q"${c.prefix}.addr == 0"

  def nonEmpty: Tree =
    q"${c.prefix}.addr != 0"

  def get =
    branchEmpty(readValue, throwEmptyRef)

  def getOrElse(default: Tree) =
    branchEmpty(readValue, default)

  def set(value: Tree) =
    branchEmpty(writeValue(value), throwEmptyRef)

  def setOrElse(value: Tree)(default: Tree) =
    branchEmpty(writeValue(value), default)

  def contains(elem: Tree) =
    branchEmpty(q"$readValue == $elem", q"false")

  def flatten(ev: Tree) = {
    val RefOf(argtpe, regtpe) = A
    if (regtpe.termSymbol != NoSymbol)
      branchEmpty(readValue, emptyRef(argtpe, regtpe))
    else
      emptyRef(argtpe, regtpe)
  }

  def map(f: Tree) =
    if (R.termSymbol != NoSymbol)
      branchEmpty(allocRef(paramTpe(f), app(f, readValue), q"${R.termSymbol}"), q"$pre")
    else
      q"${c.prefix}"

  def fold(ifEmpty: Tree)(f: Tree) =
    branchEmpty(app(f, readValue), ifEmpty)

  def filter(p: Tree) =
    stabilizedPrefix(q"if ($pre.addr == 0 || ${app(p, readValue)}) ${emptyRef(A, R)} else $pre")

  def exists(p: Tree) =
    stabilizedPrefix(q"($pre.addr != 0) && ${app(p, readValue)}")

  def forall(p: Tree) =
    stabilizedPrefix(q"($pre.addr == 0) || ${app(p, readValue)}")

  def mutate(f: Tree) =
    branchEmpty(q"$pre.set(${app(f, readValue)})", q"$pre")

  def alloc[T: WeakTypeTag](value: Tree)(r: Tree) =
    allocRef(wt[T], value, r)

  def empty[T: WeakTypeTag] =
    emptyRef(wt[T], NothingTpe)
}

class Region(val c: Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def alloc[T: WeakTypeTag](f: Tree) = {
    val r = fresh("r")
    val res = fresh("res")
    val fapp = f match {
      case q"($_ => $_)" => app(f, q"$r")
      case _             => q"$f($r)"
    }
    q"""
      val $r = $runtime.allocRegion()
      val $res = $fapp
      $runtime.disposeRegion($r)
      $res
    """
  }
}
