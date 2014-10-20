package regions.internal

import scala.collection.mutable
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
      case sym: ClassSymbol if sym.annotations.exists(_.tpe.typeSymbol == internalStructClass) =>
        val args = tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.head.map { arg => (arg.name.toTermName, arg.info) }
        val buf = mutable.ListBuffer[StructField]()
        var offset = 0
        args.foreach { case (name, tpe) =>
          buf.append(StructField(name, tpe, offset))
          val q"${size: Int}" = sizeof(tpe)
          offset += size
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
      write(LongTpe, address, q"$value.loc")
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

  // --- * --- * --- * --- * ---

  def struct(annottees: Tree*): Tree = annottees match {
    case q"class $name(..$args)" :: Nil =>
      if (args.isEmpty) abort("structs require at least one argument")
      val checks = args.map {
        case q"$_ val $name: $tpt = $default" =>
          if (default.nonEmpty) abort("structs with default values are not supported")
          q"_root_.regions.internal.ensureFixedSizeAlloc[$tpt]"
      }
      val nargs = args.map { case q"$_ val $name: $tpt = $_" => q"val $name: $tpt" }
      q"""
        @_root_.regions.internal.struct case class $name(..$nargs) {
          ..$checks
        }
      """
  }

  def ensureFixedSizeAlloc[T: WeakTypeTag]: Tree = {
    val T = weakTypeOf[T]
    T match {
      case Primitive() | StructOf(_) | RefOf(_) => q""
      case _ => abort(s"$T is not fixed sized allocatable object")
    }
  }

  // --- * --- * --- * --- * ---

  def refNonEmpty[A]: Tree = q"$prefix.loc != 0"
  def refIsEmpty[A]: Tree  = q"$prefix.loc == 0"

  def refGet[A: WeakTypeTag] =  {
    val readA = read(weakTypeOf[A], q"$prefix.loc")
    q"if ($prefix.loc != 0) $readA else throw $regions.EmptyRefException"
  }

  def refSet[A: WeakTypeTag](value: Tree) = {
    val writeA = write(weakTypeOf[A], q"$prefix.loc", value)
    q"if ($prefix.loc != 0) $writeA else throw $regions.EmptyRefException"
  }

  def refCompanionApply[T: WeakTypeTag](value: Tree)(r: Tree): Tree = {
    val T = weakTypeOf[T]
    val v = fresh("v")
    val ref = fresh("ref")
    val size = T match {
      case Primitive() | RefOf(_) | StructOf(_) => sizeof(T)
      case _                                    => abort(s"allocation of $T is not supported")
    }
    q"""
      val $v = $value
      val $ref = $internal.allocMemory[$T]($r, $size)
      $ref.set($v)
      $ref
    """
  }

  def refCompanionEmpty[T: WeakTypeTag]: Tree = q"null.asInstanceOf[Ref[${weakTypeOf[T]}]]"
}
