package regions.internal
import scala.reflect.macros.whitebox.Context

class macros(val c: Context) {
  import c.universe._
  import c.universe.definitions._

  val internalStructClass = rootMirror.staticClass("regions.internal.struct")
  val RefClass = rootMirror.staticClass("regions.Ref")

  val prefix = c.prefix.tree

  val regions = q"_root_.regions"
  val internal = q"$regions.internal"
  val unsafe = q"$internal.unsafe"

  def abort(msg: String, at: Position = c.enclosingPosition): Nothing = c.abort(at, msg)

  def fresh(pre: String): TermName = TermName(c.freshName(pre))

  def read(tpe: Type, address: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val method = TermName(s"get$tpe")
      q"$unsafe.$method($address)"
    case BooleanTpe =>
      q"$unsafe.getByte($address) != ${Literal(Constant(0.toByte))}"
    case _ if tpe.typeSymbol == RefClass =>
      val targ = tpe.typeArgs.head
      val value = read(LongTpe, address)
      q"new $regions.Ref[$targ]($value)"
    case _ if tpe.typeSymbol == ArrayClass =>
      val size = fresh("size")
      val arr = fresh("arr")
      val i = fresh("i")
      val T = tpe.typeArgs.head
      val readSize = read(IntTpe, address)
      val readIth = read(T, q"$address + 4 + ${sizeof(T)} * $i")
      q"""
        val $size = $readSize
        val $arr = new _root_.scala.Array[$T]($size)
        var $i = 0
        while ($i < $size) {
          $arr($i) = $readIth
          $i += 1
        }
        $arr
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
    case _ if tpe.typeSymbol == RefClass =>
      write(LongTpe, address, q"$value.loc")
    case _ if tpe.typeSymbol == ArrayClass =>
      val v = fresh("v")
      val i = fresh("i")
      val T = tpe.typeArgs.head
      val writeSize = write(IntTpe, address, q"$v.length")
      val writeIth = write(T, q"$address + 4 + ${sizeof(T)} * $i", q"$v($i)")
      q"""
        val $v = $value
        $writeSize
        var $i = 0
        while ($i < $v.length) {
          $writeIth
          $i += 1
        }
      """
  }

  def regionId(ref: Tree): Tree = q"($ref.loc & 0x000000FF).toInt"

  def regionOffset(ref: Tree): Tree = q"$ref.loc >> 8"

  def address(ref: Tree): Tree = q"$internal.infos(${regionId(ref)}).start + ${regionOffset(ref)}"

  def sizeof(tpe: Type, length: Tree = EmptyTree): Tree = tpe match {
    case ByteTpe  | BooleanTpe             => q"1"
    case ShortTpe | CharTpe                => q"2"
    case IntTpe   | FloatTpe               => q"4"
    case LongTpe  | DoubleTpe              => q"8"
    case _ if tpe.typeSymbol == RefClass   => q"8"
    case _ if tpe.typeSymbol == ArrayClass =>
      require(length.nonEmpty)
      val T = tpe.typeArgs.head
      T.typeSymbol match {
        case sym: ClassSymbol if sym.isPrimitive =>
        case _ => abort(s"only arrays of primitives are supported at the moment")
      }
      q"4 + ${sizeof(T)} * $length"
    case StructTpe(fields) =>
      fields.map { case (_, tpe) => sizeof(tpe) }.reduce { (l, r) => q"$l + $r" }
  }

  def refApplyDynamic[T: WeakTypeTag](method: Tree)(args: Tree*): Tree = {
    val T = weakTypeOf[T]
    T.typeSymbol match {
      case sym: ClassSymbol if sym.isPrimitive =>
        (method, args) match {
          case (q""" "apply" """, Seq()) =>
            read(T, address(prefix))
          case (q""" "update" """, v +: Seq()) =>
            if (!(v.tpe <:< T)) abort(s"updated value must be of type $T", at = v.pos)
            write(T, address(prefix), v)
        }
      case sym if sym == ArrayClass =>
        (method, args) match {
          case (q""" "apply" """, Seq()) =>
            read(T, address(prefix))
          case (q""" "apply" """, i +: Seq()) =>
            if (!(i.tpe <:< IntTpe)) abort(s"index must be of type Int", at = i.pos)
            val targ = T.typeArgs.head
            read(targ, q"${address(prefix)} + 4 + $i * ${sizeof(targ)}")
          case (q""" "update" """, v +: Seq()) =>
            if (!(v.tpe <:< T)) abort(s"updated value must be of type $T", at = v.pos)
            write(T, address(prefix), v)
          case (q""" "update" """, i +: v +: Seq()) =>
            val targ = T.typeArgs.head
            if (!(i.tpe <:< IntTpe)) abort(s"index must be of type Int", at = i.pos)
            if (!(v.tpe <:< targ)) abort(s"updated value must be of type $targ", at = v.pos)
            write(targ, q"${address(prefix)} + 4 + $i * ${sizeof(targ)}", v)
        }
    }
  }

  def refUpdateDynamic[T: WeakTypeTag](field: Tree)(value: Tree): Tree = {
    val T = weakTypeOf[T]
    T match {
      case StructTpe(fields) =>
        val q"${fieldStr: String}" = field
        fieldInfo(fields, fieldStr).map { case (tpe: Type, offset: Tree) =>
          write(tpe, q"${address(prefix)} + $offset", value)
        }.getOrElse {
          abort(s"struct $T doesn't have field $field")
        }
    }
  }

  def fieldInfo(fields: List[(String, Type)], field: String, offset: Tree = q"0"): Option[(Type, Tree)] = fields match {
    case Nil => None
    case (f, tpe) :: _ if f == field => Some((tpe, offset))
    case (f, tpe) :: rest => fieldInfo(rest, field, q"$offset + ${sizeof(tpe)}")
  }

  def refSelectDynamic[T: WeakTypeTag](field: Tree): Tree = {
    val T = weakTypeOf[T]
    T match {
      case _ if T.typeSymbol == ArrayClass =>
        field match {
          case q""" "length" """ =>
            val targ = T.typeArgs.head
            read(IntTpe, address(prefix))
        }
      case StructTpe(fields) =>
        val q"${fieldStr: String}" = field
        fieldInfo(fields, fieldStr).map { case (tpe: Type, offset: Tree) =>
          read(tpe, q"${address(prefix)} + $offset")
        }.getOrElse {
          abort(s"struct $T doesn't have field $field")
        }
    }
  }

  def struct(annottees: Tree*): Tree = annottees match {
    case q"class $name(..$args)" :: Nil =>
      if (args.isEmpty) abort("structs require at least one argument")
      val checks = args.map {
        case q"$_ val $name: $tpt = $default" =>
          if (default.nonEmpty) abort("structs with default values are not supported")
          q"_root_.regions.internal.ensureFixedSizeAlloc[$tpt]"
      }
      q"""
        @_root_.regions.internal.struct class $name private(..$args) {
          ..$checks
        }
      """
  }

  def ensureFixedSizeAlloc[T: WeakTypeTag]: Tree = {
    val T = weakTypeOf[T]
    T match {
      case ByteTpe | ShortTpe | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe => q""
      case StructTpe(_) => q""
      case _ if T.typeSymbol == RefClass => q""
      case _ => abort(s"$T is not fixed sized allocatable object")
    }
  }

  object StructTpe {
    def unapply(tpe: Type): Option[List[(String, Type)]] = tpe.typeSymbol match {
      case sym: ClassSymbol if sym.annotations.exists(_.tpe.typeSymbol == internalStructClass) =>
        Some(tpe.members.sorted.filter { m => m.isTerm && !m.isMethod }.map { m =>
          (m.name.toString, m.info)
        })
      case _ => None
    }
  }

  def refCompanionApplyDynamic[T: WeakTypeTag](method: Tree)(args: Tree*)(region: Tree): Tree = {
    val T = weakTypeOf[T]
    (T, args) match {
      case (NothingTpe, value +: Seq()) =>
        val V = value.tpe.widen
        val v = fresh("v")
        val ref = fresh("ref")
        val size = V.typeSymbol match {
          case sym: ClassSymbol if sym.isPrimitive => sizeof(V)
          case sym if sym == ArrayClass            => sizeof(V, q"$v.length")
          case _                                   => abort(s"allocation of $V is not supported")
        }
        q"""
          val $v = $value
          val $ref = $internal.allocMemory[$V]($region, $size)
          $ref() = $v
          $ref
        """
      case (StructTpe(fields), _) =>
        val ref = fresh("ref")
        val size = sizeof(T)
        val writes: List[Tree] = fields.zip(args).map {
          case ((name, tpe), v) => q"$ref.${TermName(name)} = ($v: $tpe)"
        }
        q"""
          val $ref = $internal.allocMemory[$T]($region, $size)
          ..$writes
          $ref
      """
    }
  }
}
