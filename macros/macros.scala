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
  val FatRefClass = rootMirror.staticClass("regions.FatRef")

  val regions  = q"_root_.regions"
  val internal = q"$regions.internal"
  val runtime  = q"$internal.runtime"
  val unsafe   = q"$runtime.unsafe"

  def abort(msg: String, at: Position = c.enclosingPosition): Nothing = c.abort(at, msg)
  def wt[T: WeakTypeTag]: Type        = weakTypeOf[T]
  def fresh(pre: String): TermName    = TermName(c.freshName(pre))

  object RefOf {
    def unapply(tpe: Type): Option[Type] =
      if (tpe.typeSymbol == RefClass) Some(tpe.typeArgs.head)
      else None
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
      case Primitive() | RefOf(_) | StructOf(_) => true
      case _                                    => false
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
    case RefOf(targ) =>
      val value = read(LongTpe, address)
      q"new $regions.Ref[$targ]($value)"
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
    case RefOf(_) =>
      write(LongTpe, address, q"$value.addr")
    case StructOf(fields) =>
      val v = fresh("v")
      val writes = fields.map { f =>
        write(f.tpe, q"$address + ${f.offset}", q"$v.${f.name}")
      }
      q"val $v = $value; ..$writes"
  }

  def sizeof(tpe: Type): Int = tpe match {
    case ByteTpe  | BooleanTpe           => 1
    case ShortTpe | CharTpe              => 2
    case IntTpe   | FloatTpe             => 4
    case LongTpe  | DoubleTpe | RefOf(_) => 8
    case StructOf(fields)                => fields.map(f => sizeof(f.tpe)).sum
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

trait RefCommon extends Common {
  import c.universe._

  def MyClass: ClassSymbol

  lazy val A = c.prefix.tree.tpe.baseType(MyClass).typeArgs.head

  lazy val pre = fresh("pre")

  def stabilized(value: Tree)(f: TermName => Tree) = {
    val stable = fresh("stable")
    q"val $stable = $value; ${f(stable)}"
  }

  def stabilizedPrefix(body: Tree) =
    q"val $pre = ${c.prefix}; $body"

  def branchEmpty(nonEmpty: Tree, empty: Tree) =
    stabilizedPrefix(q"if ($pre.addr != 0) $nonEmpty else $empty")

  def nonEmpty: Tree =
    q"${c.prefix}.addr != 0"

  def isEmpty: Tree =
    q"${c.prefix}.addr == 0"

  def emptyRef(T: Type) =
    q"null.asInstanceOf[$RefClass[$T]]"

  def empty[T: WeakTypeTag] =
    emptyRef(wt[T])
}

class Ref(val c: Context) extends RefCommon {
  import c.universe._

  val MyClass = RefClass

  def readValue = read(A, q"$pre.addr")

  def writeValue(value: Tree) = write(A, q"$pre.addr", value)

  def allocRef(T: Type, value: Tree, r: Tree) = {
    val v = fresh("v")
    val ref = fresh("ref")
    val size = T match {
      case Allocatable() => sizeof(T)
      case _             => abort(s"allocation of $T is not supported")
    }
    stabilized(value) { v =>
      q"""
        val $ref = new $regions.Ref[$T]($runtime.allocMemory($r, $size))
        $ref.set($v)
        $ref
      """
    }
  }

  def throwEmptyRef =
    q"throw $regions.EmptyRefException"

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

  def flatten(ev: Tree) =
    branchEmpty(readValue, emptyRef(A.baseType(MyClass).typeArgs.head))

  def map(f: Tree)(r: Tree) =
    branchEmpty(allocRef(paramTpe(f), app(f, readValue), r), q"$pre")

  def fold(ifEmpty: Tree)(f: Tree) =
    branchEmpty(app(f, readValue), ifEmpty)

  def filter(p: Tree) =
    stabilizedPrefix(q"if ($pre.addr == 0 || ${app(p, readValue)}) ${emptyRef(A)} else $pre")

  def exists(p: Tree) =
    stabilizedPrefix(q"($pre.addr != 0) && ${app(p, readValue)}")

  def forall(p: Tree) =
    stabilizedPrefix(q"($pre.addr == 0) || ${app(p, readValue)}")

  def mutate(f: Tree) =
    branchEmpty(q"$pre.set(${app(f, readValue)})", q"$pre")

  def alloc[T: WeakTypeTag](value: Tree)(r: Tree) =
    allocRef(wt[T], value, r)
}

class FatRef(val c: Context) extends RefCommon {
  import c.universe._
  import c.universe.definitions._

  val MyClass = FatRefClass

  def elementSize(T: Type) = T match {
    case Allocatable() => sizeof(T)
    case _             => abort(s"allocation of $T is not supported")
  }

  def readElement(index: Tree) =
    read(A, q"$pre.addr + 8 + $index * ${sizeof(A)}")

  def writeElement(index: Tree, value: Tree) =
    write(A, q"$pre.addr + 8 + $index * ${sizeof(A)}", value)

  def writeLength(value: Tree) =
    write(LongTpe, q"$pre.addr", value)

  def boundsChecked(index: Tree, ifOk: TermName => Tree) = stabilizedPrefix {
    val size = fresh("size")
    stabilized(index) { idx =>
      q"""
        val $size = ${read(LongTpe, q"$pre.addr")}
        if ($idx >= 0 && $idx < $size)  ${ifOk(idx)}
        else throw new _root_.java.lang.IndexOutOfBoundsException($index.toString)
      """
    }
  }

  def apply(index: Tree) =
    boundsChecked(index, i => readElement(q"$i"))

  def update(index: Tree, value: Tree) =
    boundsChecked(index, i => writeElement(q"$i", value))

  def alloc[T: WeakTypeTag](values: Tree*)(r: Tree) = {
    val T = wt[T]
    val esize = elementSize(T)
    val size = 8 + esize * values.length
    val updateValues = values.zipWithIndex.map { case (value, i) =>
      q"$pre($i) = $value"
    }
    q"""
      val $pre = new $regions.FatRef[$T]($runtime.allocMemory($r, $size))
      ${writeLength(q"${values.length}")}
      ..$updateValues
      $pre
    """
  }

  def fill[T: WeakTypeTag](n: Tree)(elem: Tree)(r: Tree) = stabilized(n) { length =>
    val i = fresh("i")
    val T = wt[T]
    val esize = elementSize(T)
    val size = q"8 + $esize * $length"
    q"""
      val $pre = new $regions.FatRef[$T]($runtime.allocMemory($r, $size))
      ${writeLength(q"$length")}
      var $i = 0L
      while ($i < $length) {
        $pre($i) = $elem
        $i += 1
      }
      $pre
    """
  }
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
