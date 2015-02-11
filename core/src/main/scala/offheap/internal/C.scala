package offheap
package internal

import scala.annotation.StaticAnnotation
import scala.language.dynamics
import scala.language.experimental.{macros => CanMacro}

object C {
  final class Ptr[T](val addr: Long) extends AnyVal with Dynamic {
    def apply(): T                                  = macro macros.Ptr.apply
    def apply(n: Long): T                           = macro macros.Ptr.applyN
    def at(n: Long): Ptr[T]                         = macro macros.Ptr.atN
    def at(name: Symbol): Any                       = macro macros.Ptr.atName
    def update(v: T): Unit                          = macro macros.Ptr.update
    def update(n: Long, v: T): Unit                 = macro macros.Ptr.updateN
    def resize(n: Long): Ptr[T]                     = macro macros.Ptr.resize
    def free: Unit                                  = macro macros.Ptr.free
    def selectDynamic(name: String): Any            = macro macros.Ptr.selectDynamic
    def updateDynamic(name: String)(v: Any): Any    = macro macros.Ptr.updateDynamic
    def applyDynamic(name: String)(args: Any*): Any = macro macros.Ptr.applyDynamic
  }

  object Ptr {
    def alloc[T]: Ptr[T] =
      macro macros.Ptr.alloc[T]

    def allocArray[T](n: Long): Ptr[T] =
      macro macros.Ptr.allocArray[T]

    def copy[T](from: Ptr[T], fromIndex: Long,
                to: Ptr[T], toIndex: Long, length: Long): Unit =
      macro macros.Ptr.copy[T]
  }

  final class struct extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any =
      macro internal.macros.Annotations.struct
  }

  final class union extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any =
      macro internal.macros.Annotations.union
  }
}
