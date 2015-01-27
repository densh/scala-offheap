package regions.internal.macros
// TODO: handle non-function case

import scala.collection.mutable
import scala.reflect.macros.{whitebox, blackbox}

trait Common {
  val c: blackbox.Context
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._
  import rootMirror.{staticClass, staticPackage}

  val OffheapClass       = staticClass("regions.internal.rt.Offheap")
  val LayoutClass        = staticClass("regions.internal.rt.Layout")
  val RefClass           = staticClass("regions.Ref")
  val RegionClass        = staticClass("regions.Region")
  val StringBuilderClass = staticClass("scala.collection.mutable.StringBuilder")

  val regions  = staticPackage("regions")
  val internal = staticPackage("regions.internal")
  val rt       = staticPackage("regions.internal.rt")
  val ct       = staticPackage("regions.internal.ct")
  val unsafe   = q"$rt.unsafe"

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

  object ClassOf {
    def is(sym: Symbol): Boolean =
      sym.annotations.exists(_.tpe.typeSymbol == OffheapClass)

    def unapply(tpe: Type): Option[List[Field]] =
      unapply(tpe.typeSymbol)

    def unapply(sym: Symbol): Option[List[Field]] = sym match {
      case sym: ClassSymbol if ClassOf.is(sym) =>
        val q"new $_(..$descriptors)" = sym.asType.toType.members.collectFirst {
          case meta if meta.name == TermName("$meta$") =>
            meta.annotations.collectFirst {
              case ann if ann.tpe.typeSymbol == LayoutClass =>
                ann
            }.get
        }.get.tree
        val buf = mutable.ListBuffer[Field]()
        var offset = 0
        descriptors.foreach { case q"(${name: String}, new $_[$tpt]())" =>
          buf.append(Field(name, tpt.tpe, offset))
          offset += sizeof(tpt.tpe)
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
    case ClassOf(fields) =>
      val sym = tpe.typeSymbol
      val R = tpe.typeArgs.head
      q"${sym.companion}.fromAddress$$unsafe[$R]($unsafe.getLong($address))"
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
    case ClassOf(fields) =>
      val sym = tpe.typeSymbol
      q"$unsafe.putLong($address, ${sym.companion}.toAddress$$unsafe($value))"
  }

  def sizeof(tpe: Type): Int = {
    assert(tpe != null)
    tpe match {
      case ByteTpe  | BooleanTpe             => 1
      case ShortTpe | CharTpe                => 2
      case IntTpe   | FloatTpe               => 4
      case LongTpe  | DoubleTpe              => 8
      case tpe if ClassOf.is(tpe.typeSymbol) => 8
    }
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
}

class Annotations(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  // TODO: handle generics
  // TODO: handle implicit parameters
  // TODO: hygienic reference to class type from companion?
  // TODO: handle mods properly
  // TODO: support classes with existing companions
  // TODO: move layout annotation to $meta$ member
  def region(annottees: Tree*): Tree = annottees match {
    case q"$mods def $name[..$targs](...$argss)(implicit ..$impl): $tpt = $body" :: Nil =>
      val q"new $_(${regionGivenName: TermName})" = c.prefix.tree
      val r = fresh("r")
      val R = regionGivenName.toTypeName
      q"""
        $mods def $name[..$targs, $R <: $RegionClass[_]]
                       (...$argss)(implicit ..$impl, $r: $R): $tpt = $body
      """
    case q"class $name(..$args) { ..$stats }" :: Nil =>
      if (args.isEmpty)
        abort("offheap classes require at least one parameter")
      val q"new $_(${regionGivenName: TermName})" = c.prefix.tree
      val r = fresh("r")
      val R = regionGivenName.toTypeName
      val addrName = TermName("$addr$")
      val addr = q"this.$addrName"
      val asserts = args.map { case q"$_ val $name: $tpt = $default" =>
        if (default.nonEmpty) abort("offheap classes don't support default arguments")
        q"$ct.assertAllocatable[$tpt]"
      }
      def throwNullRef = q"throw $regions.NullRefException"
      def throwInaccRegion = q"throw $regions.InaccessibleRegionException"
      val accessors = args.map { case q"$_ val $argName: $tpt = $_" =>
        q"""
          def $argName(implicit $r: $R): $tpt =
            if (this.isEmpty) $throwNullRef
            else if ($r == null || $r.isClosed) $throwInaccRegion
            else $ct.uncheckedAccessor[$name[$R], $tpt]($addr, ${argName.toString})
        """
      }
      val methods: List[Tree] = stats.map {
        case q"$_ def $methodName[..$targs](...$argss)(implicit ..$impl): $tpt = $body" =>
          if (tpt.isEmpty)
            abort("offheap class method require explicit return type annotations")
          val uncheckedMethodName = TermName(methodName.toString + "$unchecked")
          val targNames = targs.map { case q"$_ type $name[..$_] = $_" => name }
          val argNamess = argss.map { _.map { case q"$_ val $name: $_ = $_" => name } }
          val implNames = impl.map { case q"$_ val $name: $_ = $_" => name }
          q"""
            def $methodName[..$targs](...$argss)(implicit ..$impl, $r: $R): $tpt =
              if (this.isEmpty) $throwNullRef
              else if ($r == null || $r.isClosed) $throwInaccRegion
              else $body
          """
        case other =>
          abort(s"offheap classes may only contain methods ($other)")
      }
      val argNames = args.map { case q"$_ val $name: $_ = $_" => name }
      val instance = fresh("instance")
      val address = fresh("address")
      val scrutinee = fresh("scrutinee")
      val layout = {
        val tuples = args.map { case q"$_ val $name: $tpt = $_" =>
          q"(${name.toString}, new $rt.Tag[$tpt]())"
        }
        q"new $rt.Layout(..$tuples)"
      }
      val C = fresh("C").toTypeName
      val canMacro = fresh("canMacro")
      debug("region")(q"""
        @$rt.Offheap final class $name[$R <: $RegionClass[_]] private(
          private val $addrName: $LongClass
        ) extends $AnyValClass with $RefClass[$R] {
          import scala.language.experimental.{macros => $canMacro}
          @$layout def $$meta$$ = { ..$asserts }
          def isEmpty = $addr == 0
          def nonEmpty = $addr != 0
          ..$accessors
          ..$methods
        }
        object ${name.toTermName} {
          def apply[$R <: $RegionClass[_]](..$args)(implicit $r: $R): $name[$R] =
            $ct.allocClass[$R, $name[$R]]($r, ..$argNames)

          def unapply[$C <: $name[_]]($scrutinee: $C): $C =
            $scrutinee

          def fromAddress$$unsafe[$R <: $RegionClass[_]]($address: $LongClass): $name[$R] =
            new $name[$R]($address)

          def toAddress$$unsafe($instance: $name[_]): $LongClass =
            $instance.$addrName
        }
      """)
  }
}

class Ct(val c: blackbox.Context) extends Common {
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._

  def assertAllocatable[T: WeakTypeTag]: Tree = {
    val T = wt[T]
    T match {
      case Allocatable() => q""
      case _             => abort(s"$T is not fixed sized allocatable object")
    }
  }

  def uncheckedAcessor[C: WeakTypeTag, T: WeakTypeTag](addr: Tree, name: Tree): Tree = {
    val C = wt[C]
    val ClassOf(fields) = C
    val q"${nameStr: String}" = name
    fields.collectFirst {
      case f if f.name.toString == nameStr =>
        read(f.tpe, q"$addr + ${f.offset}")
    }.getOrElse {
      abort(s"$C ($fields) doesn't have field `$nameStr`")
    }
  }

  def capturingAccessor: Tree = {
    val C = c.prefix.tree.tpe
    val R = C.typeArgs.head
    val ClassOf(fields) = C
    val q"$_.$name" = c.macroApplication
    val r = c.inferImplicitValue(R, silent = true)
    if (r.isEmpty) abort(s"couldn't find implicit value of type $R")
    fields.zipWithIndex.collectFirst {
      case (f, i) if s"_${i+1}" == name.toString =>
        q"${c.prefix}.${TermName(f.name)}"
    }.get
  }

  def allocClass[C: WeakTypeTag](r: Tree, args: Tree*): Tree = {
    val C = wt[C]
    val ClassOf(fields) = C.typeSymbol
    val size = fields.map(f => sizeof(f.tpe)).sum
    val addr = fresh("addr")
    val writes = fields.zip(args).map { case (f, arg) =>
      write(f.tpe, q"$addr + ${f.offset}", arg)
    }
    q"""
      val $addr = $rt.allocMemory($r, $size)
      ..$writes
      new $C($addr)
    """
  }
}

class FreshId(val c: whitebox.Context) extends Common {
  import c.universe._

  def materialize = {
    import compat._
    val id = fresh("").toString.replace("$macro$", "").toInt
    q"null.asInstanceOf[FreshId[${ConstantType(Constant(id))}]]"
  }
}

class Region(val c: blackbox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def alloc(f: Tree)(id: Tree) = {
    val r = freshVal("r", tpe = RegionClass.toType, value =q"$rt.allocRegion()")
    val res = fresh("res")
    q"""
      $r
      val $res =
        try ${app(f, q"${r.symbol}")}
        finally $rt.disposeRegion(${r.symbol})
      $res
    """
  }


}
