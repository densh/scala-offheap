import scala.language.dynamics
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

package regions {
  final class Region(val id: Int) extends AnyVal with Dynamic {
    def dispose() = internal.disposeRegion(this)
  }
  object Region {
    def apply() = internal.allocRegion()
  }

  final class Ref[T](val loc: Long) extends AnyVal with Dynamic {
    def applyDynamic(method: String)(args: Any*): Any = macro internal.macros.refApplyDynamic[T]
    def updateDynamic(field: String)(value: Any): Any = macro internal.macros.refUpdateDynamic[T]
    def selectDynamic(field: String): Any             = macro internal.macros.refSelectDynamic[T]
    def nonEmpty: Boolean = loc != 0
    def isEmpty: Boolean  = loc == 0
  }
  object Ref extends Dynamic {
    def applyDynamic[T](method: String)(args: Any*)(implicit region: Region): Any = macro internal.macros.refCompanionApplyDynamic[T]
    def empty[T]: Ref[T] = null.asInstanceOf[Ref[T]]
  }

  class struct extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro internal.macros.struct
  }
}
