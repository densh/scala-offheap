package regions.internal.macros

import scala.collection.mutable
import scala.reflect.macros.{whitebox, blackbox}

trait Common {
  val c: blackbox.Context
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  val OffheapClass      = rootMirror.staticClass("regions.internal.runtime.offheap")
  val RefClass          = rootMirror.staticClass("regions.Ref")
  val UnwrappedRefClass = rootMirror.staticClass("regions.UnwrappedRef")

  val regions  = q"_root_.regions"
  val internal = q"$regions.internal"
  val runtime  = q"$internal.runtime"
  val unsafe   = q"$runtime.unsafe"

  def abort(msg: String, at: Position = c.enclosingPosition): Nothing = c.abort(at, msg)
  def fresh(pre: String): TermName = TermName(c.freshName(pre))
  def debug[T](header: String)(f: => T): T = {
    val res = f
    println(s"$header = $res")
    res
  }

  object RefOf {
    def unapply(tpe: Type): Option[Type] = {
      val base = tpe.typeSymbol match {
        case RefClass | UnwrappedRefClass => Some(tpe.baseType(tpe.typeSymbol))
        case _                            => None
      }
      base.map(_.typeArgs.head)
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
      case Primitive() | RefOf(_) | ClassOf(_) => true
      case _                                    => false
    }
  }

  case class StructField(name: TermName, tpe: Type, offset: Long)

  object ClassOf {
    def unapply(tpe: Type): Option[List[StructField]] = tpe.typeSymbol match {
      case sym: ClassSymbol if sym.annotations.exists(_.tpe.typeSymbol == OffheapClass) =>
        val args = tpe.typeSymbol.asClass.primaryConstructor
                      .asMethod.paramLists.head.map { arg =>
          (arg.name.toTermName, arg.info)
        }
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
    case ClassOf(fields) =>
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
    case ClassOf(fields) =>
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
    case ClassOf(fields)                => fields.map(f => sizeof(f.tpe)).sum
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

class Annotations(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def offheapName(name: Name) =
    TermName("$offheap$" + name.toString)

  // TODO: turn off stabilization for offheap accessors
  // TODO: handle mods properly
  // TODO: handle generics
  // TODO: hygienic reference to class type from companion
  // TODO: transform this to $self in offheap methods
  def offheap(annottees: Tree*): Tree = debug("@offheap")(annottees match {
    case q"class $name(..$args) { ..$stats }" :: Nil =>
      if (args.isEmpty)
        abort("offheap classes require at least one parameter")
      val checks = args.map {
        case q"$_ val $name: $tpt = $default" =>
          if (default.nonEmpty) abort("structs with default values are not supported")
          q"$internal.Ensure.allocatable[$tpt]"
      }
      val self = fresh("self")
      val Self = tq"$UnwrappedRefClass[$name]"
      val offheapAccessors: List[Tree] = args.map {
        case q"$_ val $name: $tpt" =>
          q"def ${offheapName(name)}($self: $Self): $tpt = $self.$name"
      }
      val offheapScope: List[Tree] = {
        val aliasAcessors: List[Tree] = args.map {
          case q"$_ val $name: $tpt" =>
            q"def $name: $tpt = ${offheapName(name)}($self)"
        }
        val aliasMethods: List[Tree] = stats.collect {
          case q"$_ def $name(...$args): $tpt = $body" =>
            val argNames = args.map { _.map { case q"$_ val $name: $_ = $_" => name } }
            q"def $name(...$args): $tpt = ${offheapName(name)}($self)(...$argNames)"
        }
        aliasAcessors ++ aliasMethods
      }
      val offheapMethods: List[Tree] = stats.map {
        case q"$_ def $name(...$args): $tpt = $body" =>
          q"""
            def ${offheapName(name)}($self: $Self)(...$args): $tpt = {
              ..$offheapScope;
              $body
            }
          """
        case m => abort("unsupported member", at = m.pos)
      }
      val argNames = args.map(_.name)
      val r = fresh("r")
      q"""
        @$runtime.offheap final class $name private(..$args) {
          ..$stats
          ..$checks
        }
        object ${name.toTermName} {
          ..$offheapAccessors
          ..$offheapMethods
          def apply(..$args)(implicit $r: $regions.Region): $Self =
            $runtime.allocClass[$name]($r, ..$argNames)
          def unapply($self: $Self): $Self = $self
        }
      """
  })
}

class Ensure(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
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
  import c.universe.{ weakTypeOf => wt, _ }

  lazy val A   = RefOf.unapply(c.prefix.tree.tpe).get
  lazy val pre = fresh("pre")

  def ThisRefClass: ClassSymbol

  def stabilized(value: Tree)(f: TermName => Tree) = {
    val stable = fresh("stable")
    q"val $stable = $value; ${f(stable)}"
  }

  def stabilizedPrefix(body: Tree) =
    q"val $pre = ${c.prefix}; $body"

  def branchEmpty(nonEmpty: Tree, empty: Tree) =
    stabilizedPrefix(q"if ($pre.addr != 0) $nonEmpty else $empty")

  def throwEmptyRef =
    q"throw $regions.EmptyRefException"

  def emptyRef(T: Type) =
    q"null.asInstanceOf[$ThisRefClass[$T]]"

  def allocRef(T: Type, value: Tree, r: Tree) = {
    val v = fresh("v")
    val ref = fresh("ref")
    val size = T match {
      case Allocatable() => sizeof(T)
      case _             => abort(s"allocation of $T is not supported")
    }
    stabilized(value) { v =>
      q"""
        val $ref = new $ThisRefClass[$T]($runtime.allocMemory($r, $size))
        ${write(T, q"$ref.addr", q"$v")}
        $ref
      """
    }
  }

  def alloc[T: WeakTypeTag](value: Tree)(r: Tree) =
    allocRef(wt[T], value, r)

  def empty[T: WeakTypeTag] =
    emptyRef(wt[T])

  def isEmpty: Tree =
    q"${c.prefix}.addr == 0"

  def nonEmpty: Tree =
    q"${c.prefix}.addr != 0"
}

class Ref(val c: blackbox.Context) extends RefCommon {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  lazy val ThisRefClass = RefClass

  def readValue = read(A, q"$pre.addr")

  def writeValue(value: Tree) = write(A, q"$pre.addr", value)

  def get =
    branchEmpty(readValue, throwEmptyRef)

  def getOrElse(default: Tree) =
    branchEmpty(readValue, default)

  def contains(elem: Tree) =
    branchEmpty(q"$readValue == $elem", q"false")

  def flatten(ev: Tree) = {
    val RefOf(argtpe) = A
    branchEmpty(readValue, emptyRef(argtpe))
  }

  def map(f: Tree)(r: Tree) =
    branchEmpty(allocRef(paramTpe(f), app(f, readValue), r), q"$pre")

  def fold(ifEmpty: Tree)(f: Tree) =
    branchEmpty(app(f, readValue), ifEmpty)

  def filter(p: Tree) =
    stabilizedPrefix(q"""
      if ($pre.addr == 0 || ${app(p, readValue)}) ${emptyRef(A)} else $pre
    """)

  def exists(p: Tree) =
    stabilizedPrefix(q"($pre.addr != 0) && ${app(p, readValue)}")

  def forall(p: Tree) =
    stabilizedPrefix(q"($pre.addr == 0) || ${app(p, readValue)}")
}

class UnwrappedRef(val c: whitebox.Context) extends RefCommon {
  import c.universe._

  lazy val ThisRefClass = UnwrappedRefClass

  def applyDynamic(method: Tree)(args: Tree*): Tree = ???

  def selectDynamic(field: Tree): Tree =
    branchEmpty(nonEmpty = {
      A match {
        case ClassOf(fields) =>
          val q"${fieldStr: String}" = field
          fields.collectFirst {
            case f if f.name.decoded.toString == fieldStr =>
              read(f.tpe, q"$pre.addr + ${f.offset}")
          }.getOrElse {
            abort(s"class $A doesn't have field $field")
          }
      }
    }, empty = throwEmptyRef)
}

class Region(val c: blackbox.Context) extends Common {
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

class Runtime(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }

  def allocClass[T: WeakTypeTag](r: Tree, args: Tree*): Tree = {
    val ref = fresh("ref")
    val T = wt[T]
    val ClassOf(fields) = T
    val size = sizeof(T)
    val writes = fields.zip(args).map { case (f, arg) =>
      write(f.tpe, q"$ref.addr + ${f.offset}", arg)
    }
    q"""
      val $ref = new $UnwrappedRefClass[$T]($runtime.allocMemory($r, $size))
      ..$writes
      $ref
    """
  }
}
