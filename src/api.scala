import scala.language.dynamics
import scala.language.experimental.macros
import scala.collection.immutable.IntMap

package regions {
  final class Region private[regions](val id: Int) extends AnyVal {
    def alloc[T](value: T): Ref[T] = macro internal.macros.alloc[T]
  }

  final class Ref[T] private[regions](val loc: Long) extends AnyVal with Dynamic {
    def applyDynamic(method: String)(args: Any*): Any = macro internal.macros.refApplyDynamic[T]
    def updateDynanic(field: String)(value: Any): Any = macro internal.macros.refUpdateDynamic[T]
    def selectDynamic(field: String): Any             = macro internal.macros.refSelectDynamic[T]
  }
  object Ref {
    def empty[T]: Ref[T] = null.asInstanceOf[Ref[T]]
  }
}

package object regions {
  def withRegion[T](f: Region => T): T = {
    val region = internal.allocRegion()
    try f(region)
    finally internal.disposeRegion(region)
  }
}
