package regions.internal.macros
// TODO: handle non-function case

import scala.collection.mutable
import scala.reflect.macros.{whitebox, blackbox}

trait Common {
  val c: blackbox.Context
  import c.universe.{ weakTypeOf => wt, _ }
  import c.universe.definitions._
  import rootMirror.{staticClass, staticPackage}

  val OffheapClass       = staticClass("regions.internal.rt.offheap")
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

  object ClassOf {
    def is(sym: Symbol): Boolean =
      sym.annotations.exists(_.tpe.typeSymbol == OffheapClass)
    def unapply(tpe: Type): Option[List[Field]] = unapply(tpe.typeSymbol)
    def unapply(sym: Symbol): Option[List[Field]] = sym match {
      case sym: ClassSymbol if ClassOf.is(sym) =>
        val q"new $_($_(..$descriptors))" = sym.annotations.collectFirst {
          case ann if ann.tpe.typeSymbol == OffheapClass => ann
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

  def read(tpe: Type, address: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val method = TermName(s"get$tpe")
      q"$unsafe.$method($address)"
    case BooleanTpe =>
      q"$unsafe.getByte($address) != ${Literal(Constant(0.toByte))}"
    case ClassOf(fields) =>
      val sym = tpe.typeSymbol
      q"${sym.companion}.apply$$unchecked$$($unsafe.getLong($address))"
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
      q"$unsafe.putLong($address, $value.__addr)"
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
}

class Annotations(val c: whitebox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  // TODO: handle generics
  // TODO: handle implicit parameters
  // TODO: hygienic reference to class type from companion?
  // TODO: handle mods properly
  // TODO: case-class-like utility methods
  //       https://github.com/scalamacros/paradise/blob/2.11.x/tests/src/main/scala/kase.scala
  // TODO: name-based-pat-mat support
  // TODO: support classes with existing companions
  def offheap(annottees: Tree*): Tree = annottees match {
    case q"class $name(..$args) { ..$stats }" :: Nil =>
      if (args.isEmpty)
        abort("offheap classes require at least one parameter")
      val addrName = TermName("__addr")
      val addr = q"this.$addrName"
      val asserts = args.map { case q"$_ val $name: $tpt = $default" =>
        if (default.nonEmpty) abort("offheap classes don't support default arguments")
        q"$ct.assertAllocatable[$tpt]"
      }
      def throwNullRef = q"throw $regions.NullRefException"
      val accessors = args.flatMap { case q"$_ val $argName: $tpt = $_" =>
        val uncheckedArgName = TermName(argName.toString + "$unchecked$")
        val q"..$stats" = q"""
          def $argName: $tpt =
            if (this.isEmpty) $throwNullRef
            else this.$uncheckedArgName
          def $uncheckedArgName: $tpt =
            $ct.uncheckedAccessor[$name, $tpt]($addr, ${argName.toString})
        """
        stats
      }
      val methods: List[Tree] = stats.flatMap {
        case q"$_ def $methodName[..$targs](...$argss): $tpt = $body" =>
          if (tpt.isEmpty)
            abort("offheap class method require explicit return type annotations")
          val uncheckedMethodName = TermName(methodName.toString + "$unchecked$")
          val targNames = targs.map { case q"$_ type $name[..$_] = $_" => name }
          val argNamess = argss.map { _.map { case q"$_ val $name: $_ = $_" => name } }
          val q"..$stats" = q"""
            def $methodName[..$targs](...$argss): $tpt =
              if (this.isEmpty) $throwNullRef
              else this.$uncheckedMethodName[..$targNames](...$argNamess)
            def $uncheckedMethodName[..$targs](...$argss): $tpt =
              $ct.uncheckedMethodBody[$name, $tpt]($body)
          """
          stats
        case other =>
          abort(s"offheap classes may only contain methods ($other)")
      }
      val argNames = args.map { case q"$_ val $name: $_ = $_" => name }
      val nameBasedPatMatSupport: Tree = {
        val _ns = args.zipWithIndex.map {
          case (q"$_ val $argName: $_ = $_", i) =>
            val _n = TermName("_" + (i + 1))
            q"def ${_n} = this.$argName"
        }
        q"""
          def isEmpty = $addr == 0
          def nonEmpty = $addr != 0
          def get = this
          ..${_ns}
        """
      }
      // can't generate custom equals & hashCode due to value class restrictions
      val caseClassSupport: Tree = {
        val r = fresh("r")
        val copyArgs = args.map { case q"$_ val $name: $tpt = $_" =>
          q"val $name: $tpt = this.$name"
        }
        val sb = fresh("sb")
        val appendFields = argNames.flatMap { argName =>
          List(q"$sb.append(this.$argName.toString)", q"""$sb.append(", ")""")
        }.init
        q"""
          def copy(..$copyArgs)(implicit $r: $RegionClass): $name =
            if (this.isEmpty) $throwNullRef
            else this.copy$$unchecked$$(..$argNames)($r)
          def copy$$unchecked$$(..$copyArgs)(implicit $r: $RegionClass): $name =
            ${name.toTermName}.apply(..$argNames)($r)
          override def toString(): $StringClass =
            if (this.isEmpty) $throwNullRef
            else this.toString$$unchecked$$()
          def toString$$unchecked$$(): $StringClass = {
            val $sb = new $StringBuilderClass
            $sb.append(${name.toString})
            $sb.append("(")
            ..$appendFields
            $sb.append(")")
            $sb.toString
          }
        """
      }
      val r = fresh("r")
      val scrutinee = fresh("scrutinee")
      val layout = {
        val tuples = args.map { case q"$_ val $name: $tpt = $_" =>
          q"(${name.toString}, $rt.Tag[$tpt]())"
        }
        q"$rt.Layout(..$tuples)"
      }
      debug("@offheap")(q"""
        @$rt.offheap($layout) final class $name private(val $addrName: $LongClass)
            extends $AnyValClass with $RefClass {
          def $$meta$$ = { ..$asserts }
          ..$accessors
          ..$methods
          ..$nameBasedPatMatSupport
          ..$caseClassSupport
        }
        object ${name.toTermName} {
          def apply(..$args)(implicit $r: $RegionClass): $name =
            $ct.allocClass[$name]($r, ..$argNames)
          def apply$$unchecked$$($addrName: $LongClass): $name =
            new $name($addrName)
          def unapply($scrutinee: $name): $name = $scrutinee
          def empty: $name = null.asInstanceOf[$name]
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

  def uncheckedMethodBody[C: WeakTypeTag, T: WeakTypeTag](body: Tree): Tree = {
    import c.internal._, c.internal.decorators._
    val C = wt[C]
    val CThis = C.typeSymbol.asClass.thisPrefix
    val T = wt[T]
    typingTransform(body) { (tree, api) =>
      tree match {
        case sel @ q"$pre.$name" if pre.tpe == CThis =>
          val uncheckedName = TermName(name.toString + "$unchecked$")
          val uncheckedSymbol =
            C.members.collectFirst {
              case sym if sym.name == uncheckedName =>
                sym
            }.get
          q"$pre.$uncheckedName"
            .setType(sel.tpe)
            .setSymbol(uncheckedSymbol)
        case _ =>
          api.default(tree)
      }
    }
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

class Region(val c: blackbox.Context) extends Common {
  import c.universe._
  import c.universe.definitions._

  def alloc[T: WeakTypeTag](f: Tree) = {
    val r = freshVal("r", tpe = RegionClass.toType, value =q"$rt.allocRegion()")
    val res = fresh("res")
    q"""
      $r
      val $res = ${app(f, q"${r.symbol}")}
      $rt.disposeRegion(${r.symbol})
      $res
    """
  }
}
