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
  }

  def address(ref: Tree): Tree =
    q"$internal.regions($internal.refRegion($ref).id).start + $internal.refOffset($ref)"

  def sizeof(tpe: Type) = tpe match {
    case ByteTpe  | BooleanTpe => 1
    case ShortTpe | CharTpe    => 2
    case IntTpe   | FloatTpe   => 4
    case LongTpe  | DoubleTpe  => 8
  }

  def alloc[T: WeakTypeTag](value: Tree): Tree = {
    val T = weakTypeOf[T]
    T.typeSymbol match {
      case sym: ClassSymbol if sym.isPrimitive =>
        q"""
          val ref = $internal.allocMemory[$T]($prefix, ${sizeof(T)})
          ref() = $value
          ref
        """
    }
  }

  def refApplyDynamic[T: WeakTypeTag](method: Tree)(args: Tree*): Tree = {
    val T = weakTypeOf[T]
    T.typeSymbol match {
      case sym: ClassSymbol if sym.isPrimitive =>
        (method, args) match {
          case (q""" "apply" """, Seq()) =>
            read(T, address(prefix))
          case (q""" "update" """, v +: Seq()) =>
            if (!(v.tpe <:< T)) c.abort(v.pos, s"value must be of type $T")
            write(T, address(prefix), v)
        }
    }
  }

  def refUpdateDynamic[T](field: Tree)(value: Tree): Tree = ???

  def refSelectDynamic[T](field: Tree): Tree = ???
}
