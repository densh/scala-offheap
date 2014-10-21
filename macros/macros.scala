package regions.internal.macros

import scala.collection.mutable
import scala.reflect.macros.whitebox.Context

trait util {
  val c: Context
  import c.universe._
  import c.universe.definitions._

  val runtimeStructClass = rootMirror.staticClass("regions.internal.runtime.struct")
  val runtimeUnionClass = rootMirror.staticClass("regions.internal.runtime.union")
  val RefClass = rootMirror.staticClass("regions.Ref")
  val regions = q"_root_.regions"
  val internal = q"$regions.internal"
  val runtime = q"$internal.runtime"
  val unsafe = q"$runtime.unsafe"

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

  case class StructField(name: TermName, tpe: Type, offset: Long)

  object StructOf {
    def unapply(tpe: Type): Option[List[StructField]] = tpe.typeSymbol match {
      case sym: ClassSymbol if sym.annotations.exists(_.tpe.typeSymbol == runtimeStructClass) =>
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
    case ArrayOf(targ) =>
      val size = fresh("size")
      val arr = fresh("arr")
      val i = fresh("i")
      val readSize = read(IntTpe, address)
      val readIth = read(targ, q"$address + 4 + ${sizeof(targ)} * $i")
      q"""
        val $size = $readSize
        val $arr = new _root_.scala.Array[$targ]($size)
        var $i = 0
        while ($i < $size) {
          $arr($i) = $readIth
          $i += 1
        }
        $arr
      """
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
    case ArrayOf(targ) =>
      val v = fresh("v")
      val i = fresh("i")
      val writeSize = write(IntTpe, address, q"$v.length")
      val writeIth = write(targ, q"$address + 4 + ${sizeof(targ)} * $i", q"$v($i)")
      q"""
        val $v = $value
        $writeSize
        var $i = 0
        while ($i < $v.length) {
          $writeIth
          $i += 1
        }
      """
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

  def declosurify(f: Tree, argValue: Tree) = {
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
  }

  def maybeDeclosurify(f: Tree, argValue: Tree) = f match {
    case q"($_ => $_)" => declosurify(f, argValue)
    case _             => q"$f($argValue)"
  }

  def paramTpe(f: Tree) = f.tpe.typeArgs.head

  def stabilized(f: TermName => Tree) = {
    val pre = fresh("pre")
    q"val $pre = ${c.prefix}; ${f(pre)}"
  }
}

class annotations(val c: Context) extends util {
  import c.universe._
  import c.universe.definitions._

  def struct(annottees: Tree*): Tree = annottees match {
    case q"class $name(..$args)" :: Nil =>
      if (args.isEmpty) abort("structs require at least one argument")
      val checks = args.map {
        case q"$_ val $name: $tpt = $default" =>
          if (default.nonEmpty) abort("structs with default values are not supported")
          q"$internal.ensure.allocatable[$tpt]"
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

class ensure(val c: Context) extends util {
  import c.universe._
  import c.universe.definitions._

  def allocatable[T: WeakTypeTag]: Tree = {
    val T = wt[T]
    T match {
      case Primitive() | StructOf(_) | RefOf(_) => q""
      case _ => abort(s"$T is not fixed sized allocatable object")
    }
  }
}

class ref(val c: Context) extends util {
  import c.universe._
  import c.universe.definitions._

  def branch(f: TermName => (Tree, Tree)) =
    stabilized { pre =>
      val (nonEmpty, empty) = f(pre)
      q"if ($pre.addr != 0) $nonEmpty else $empty"
    }

  def allocRef(A: Type, value: Tree, r: Tree) = {
    val v = fresh("v")
    val ref = fresh("ref")
    val size = A match {
      case Primitive() | RefOf(_) | StructOf(_) => sizeof(A)
      case _                                    => abort(s"allocation of $A is not supported")
    }
    q"""
      val $v = $value
      val $ref = $runtime.allocMemory[$A]($r, $size)
      $ref.set($v)
      $ref
    """
  }

  def emptyRef(A: Type) =
    q"null.asInstanceOf[$RefClass[$A]]"

  def throwEmptyRef =
    q"throw $regions.EmptyRefException"

  def nonEmpty[A]: Tree =
    q"${c.prefix}.addr != 0"

  def isEmpty[A]: Tree =
    q"${c.prefix}.addr == 0"

  def get[A: WeakTypeTag] =
    branch { pre => (read(wt[A], q"$pre.addr"), throwEmptyRef) }

  def getOrElse[A: WeakTypeTag](default: Tree) =
    branch { pre => (read(wt[A], q"$pre.addr"), default) }

  def set[A: WeakTypeTag](value: Tree) =
    branch { pre => (write(wt[A], q"$pre.addr", value), throwEmptyRef) }

  def setOrElse[A: WeakTypeTag](value: Tree)(default: Tree) =
    branch { pre => (write(wt[A], q"$pre.addr", value), default) }

  def contains[A: WeakTypeTag](elem: Tree) =
    branch { pre =>
      val readA = read(wt[A], q"$pre.addr")
      (q"$readA == $elem", q"false")
    }

  def flatten[A: WeakTypeTag](ev: Tree) =
    branch { pre => (read(wt[A], q"$pre.addr"), emptyRef(wt[A].typeArgs.head)) }

  def map[A: WeakTypeTag](f: Tree)(r: Tree) =
    branch { pre =>
      (allocRef(paramTpe(f), maybeDeclosurify(f, read(wt[A], q"$pre.addr")), r), q"$pre")
    }

  def fold[A: WeakTypeTag](ifEmpty: Tree)(f: Tree) =
    branch { pre => (maybeDeclosurify(f, read(wt[A], q"$pre.addr")), ifEmpty) }

  def alloc[A: WeakTypeTag](value: Tree)(r: Tree) =
    allocRef(wt[A], value, r)

  def empty[A: WeakTypeTag] =
    emptyRef(wt[A])
}

class region(val c: Context) extends util {
  import c.universe._
  import c.universe.definitions._

  def alloc[T: WeakTypeTag](f: Tree) = {
    val r = fresh("r")
    val res = fresh("res")
    val app = f match {
      case q"($_ => $_)" => declosurify(f, q"$r")
      case _             => q"$f($r)"
    }
    q"""
      val $r = $runtime.allocRegion()
      val $res = $app
      $runtime.disposeRegion($r)
      $res
    """
  }
}
