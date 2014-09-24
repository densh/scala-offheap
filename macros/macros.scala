package regions.internal
import scala.reflect.macros.whitebox.Context

class macros(val c: Context) {
  import c.universe._
  import c.universe.definitions._

  val prefix = c.prefix.tree

  val internal = q"_root_.regions.internal"

  val unsafe = q"$internal.unsafe"

  def read(tpe: Type, address: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val method = TermName(s"get$tpe")
      q"$unsafe.$method($address)"
    case BooleanTpe =>
      q"$unsafe.getByte($address) != ${Literal(Constant(0.toByte))}"
    case _ if tpe.typeSymbol == ArrayClass =>
      val T = tpe.typeArgs.head
      val size = read(IntTpe, address)
      val ith = read(T, q"$address + 4 + ${sizeof(T)} * i")
      q"""
        val size = $size
        val arr = new _root_.scala.Array[$T](size)
        var i = 0
        while (i < size) {
          arr(i) = $ith
          i += 1
        }
        arr
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
    case _ if tpe.typeSymbol == ArrayClass =>
      val T = tpe.typeArgs.head
      val writeSize = write(IntTpe, address, q"$value.length")
      val writeIth = write(T, q"$address + 4 + ${sizeof(T)} * i", q"$value(i)")
      q"""
        $writeSize
        var i = 0
        while (i < $value.length) {
          $writeIth
          i += 1
        }
      """
  }

  def address(ref: Tree): Tree =
    q"$internal.regions($internal.refRegion($ref).id).start + $internal.refOffset($ref)"

  def sizeof(tpe: Type, length: Tree = EmptyTree): Tree = tpe match {
    case ByteTpe  | BooleanTpe             => q"1"
    case ShortTpe | CharTpe                => q"2"
    case IntTpe   | FloatTpe               => q"4"
    case LongTpe  | DoubleTpe              => q"8"
    case _ if tpe.typeSymbol == ArrayClass =>
      require(length.nonEmpty)
      val T = tpe.typeArgs.head
      T.typeSymbol match {
        case sym: ClassSymbol if sym.isPrimitive =>
        case _ => c.abort(c.enclosingPosition, s"only arrays of primitives are supported at the moment")
      }
      q"4 + ${sizeof(T)} * $length"
  }

  def alloc[T: WeakTypeTag](value: Tree): Tree = {
    val T = weakTypeOf[T]
    val size = T.typeSymbol match {
      case sym: ClassSymbol if sym.isPrimitive => sizeof(T)
      case sym if sym == ArrayClass            => sizeof(T, q"$value.length")
      case _                                   => c.abort(c.enclosingPosition, "allocation of $T is not supported")
    }
    q"""
      val ref = $internal.allocMemory[$T]($prefix, $size)
      ref() = $value
      ref
    """
  }

  def refApplyDynamic[T: WeakTypeTag](method: Tree)(args: Tree*): Tree = {
    val T = weakTypeOf[T]
    T.typeSymbol match {
      case sym: ClassSymbol if sym.isPrimitive =>
        (method, args) match {
          case (q""" "apply" """, Seq()) =>
            read(T, address(prefix))
          case (q""" "update" """, v +: Seq()) =>
            if (!(v.tpe <:< T)) c.abort(v.pos, s"updated value must be of type $T")
            write(T, address(prefix), v)
        }
      case sym if sym == ArrayClass =>
        (method, args) match {
          case (q""" "apply" """, Seq()) =>
            read(T, address(prefix))
          case (q""" "apply" """, i +: Seq()) =>
            if (!(i.tpe <:< IntTpe)) c.abort(i.pos, s"index must be of type Int")
            val targ = T.typeArgs.head
            read(targ, q"${address(prefix)} + 4 + $i * ${sizeof(targ)}")
          case (q""" "update" """, v +: Seq()) =>
            if (!(v.tpe <:< T)) c.abort(v.pos, s"updated value must be of type $T")
            write(T, address(prefix), v)
          case (q""" "update" """, i +: v +: Seq()) =>
            val targ = T.typeArgs.head
            if (!(i.tpe <:< IntTpe)) c.abort(i.pos, s"index must be of type Int")
            if (!(v.tpe <:< targ)) c.abort(v.pos, s"updated value must be of type $targ")
            write(targ, q"${address(prefix)} + 4 + $i * ${sizeof(targ)}", v)
        }
    }
  }

  def refUpdateDynamic[T](field: Tree)(value: Tree): Tree = ???

  def refSelectDynamic[T](field: Tree): Tree = ???
}
