import scala.language.dynamics
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

package regions {
  final class Region private[regions](val id: Int) extends AnyVal with Dynamic {
    def applyDynamic[T](method: String)(args: Any*): Any                = macro internal.macros.regionApplyDynamic[T]
    def applyDynamicNamed[T](method: String)(args: (String, Any)*): Any = macro internal.macros.regionApplyDynamicNamed[T]
  }

  final class Ref[T] private[regions](val loc: Long) extends AnyVal with Dynamic {
    def applyDynamic(method: String)(args: Any*): Any = macro internal.macros.refApplyDynamic[T]
    def updateDynanic(field: String)(value: Any): Any = macro internal.macros.refUpdateDynamic[T]
    def selectDynamic(field: String): Any             = macro internal.macros.refSelectDynamic[T]
  }
  object Ref {
    def empty[T]: Ref[T] = null.asInstanceOf[Ref[T]]
  }

  class struct extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro internal.macros.struct
  }
}

package object regions {
  def withRegion[T](f: Region => T): T = {
    val region = internal.allocRegion()
    try f(region)
    finally internal.disposeRegion(region)
  }
}
