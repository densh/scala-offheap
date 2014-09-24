package regions.internal
import scala.reflect.macros.whitebox.Context

class macros(val c: Context) {
  import c.universe._
  import c.universe.definitions._

  val prefix = c.prefix.tree

  val internal = q"_root_.regions.internal"

  val unsafe = q"$internal.unsafe"

  def readPrimitive(tpe: Type, address: Tree): Tree = {
    val method = TermName(tpe match {
      case ByteTpe  => "getByte"
      case ShortTpe => "getShort"
      case IntTpe   => "getInt"
      case LongTpe  => "getLong"
    })
    q"$unsafe.$method($address)"
  }

  def writePrimitive(tpe: Type, address: Tree, value: Tree): Tree = {
    val method = TermName(tpe match {
      case ByteTpe  => "putByte"
      case ShortTpe => "putShort"
      case IntTpe   => "putInt"
      case LongTpe  => "putLong"
    })
    q"$unsafe.$method($address, $value)"
  }

  def address(ref: Tree) = q"$internal.regions($internal.refRegion($ref).id).start + $internal.refOffset($ref)"

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
          case ByteTpe  => 1
          case ShortTpe => 2
          case IntTpe   => 4
          case LongTpe  => 8
        }
        q"$internal.allocMemory[$T]($prefix, $size)"
    }
  }
}
