package regions.internal
import scala.reflect.macros.whitebox.Context

class macros(val c: Context) {
  import c.universe._
  import c.universe.definitions._

  val prefix = c.prefix.tree

  val internal = q"_root_.regions.internal"

  val unsafe = q"$internal.unsafe"

  def readPrimitive(tpe: Type, address: Tree): Tree = tpe match {
    case ByteTpe | ShortTpe  | IntTpe | LongTpe | FloatTpe | DoubleTpe | CharTpe =>
      val method = TermName(s"get$tpe")
      q"$unsafe.$method($address)"
    case BooleanTpe =>
      q"$unsafe.getByte($address) != ${Literal(Constant(0.toByte))}"
  }

  def writePrimitive(tpe: Type, address: Tree, value: Tree): Tree = tpe match {
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

  def refApplyDynamic[T: WeakTypeTag](method: Tree)(args: Tree*): Tree = {
    val T = weakTypeOf[T]
    T.typeSymbol match {
      case sym: ClassSymbol if sym.isPrimitive =>
        (method, args) match {
          case (q""" "apply" """, Seq()) =>
            readPrimitive(T, address(prefix))
          case (q""" "update" """, v +: Seq()) =>
            if (!(v.tpe <:< T)) c.abort(v.pos, s"value must be of type $T")
            writePrimitive(T, address(prefix), v)
        }
    }
  }

  def refUpdateDynamic[T](field: Tree)(value: Tree): Tree = { println(s"refUpdateDynamic($field)($value)"); ??? }

  def refSelectDynamic[T](field: Tree): Tree = { println(s"refSelectDynamic($field)"); ??? }

  def alloc[T: WeakTypeTag](): Tree = {
    val T = weakTypeOf[T]
    T.typeSymbol match {
      case sym: ClassSymbol if sym.isPrimitive =>
        val size = T match {
          case ByteTpe  | BooleanTpe => 1
          case ShortTpe | CharTpe    => 2
          case IntTpe   | FloatTpe   => 4
          case LongTpe  | DoubleTpe  => 8
        }
        q"$internal.allocMemory[$T]($prefix, $size)"
    }
  }
}
