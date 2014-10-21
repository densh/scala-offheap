import scala.language.dynamics
import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation
import regions.internal.{runtime, macros}

package regions {
  class Region private[regions](
    private[regions] var node: runtime.Node,
    private[regions] var offset: Long
  )
  object Region {
    def apply[T](f: Region => T): T = macro internal.macros.region.alloc[T]
  }

  final class Ref[A](val addr: Long) extends AnyVal {
    def nonEmpty: Boolean                             = macro macros.ref.nonEmpty
    def isEmpty: Boolean                              = macro macros.ref.isEmpty
    def get: A                                        = macro macros.ref.get
    def getOrElse[B >: A](default: => B): B           = macro macros.ref.getOrElse
    def set(value: A): Unit                           = macro macros.ref.set
    def setOrElse(value: A)(default: => Unit): Unit   = macro macros.ref.setOrElse
    def contains[A1 >: A](elem: A1): Boolean          = macro macros.ref.contains
    def flatten[B](implicit ev: A <:< Ref[B]): Ref[B] = macro macros.ref.flatten
    def map[B](f: A => B)(implicit r: Region): Ref[B] = macro macros.ref.map
    def fold[B](ifEmpty: B)(f: A => B): B             = macro macros.ref.fold
    def filter(p: A => Boolean): Ref[A]               = macro macros.ref.filter
    def exists(p: A => Boolean): Boolean              = macro macros.ref.exists
    def forall(p: A => Boolean): Boolean              = macro macros.ref.forall
    def mutate[B](f: A => B): Unit                    = macro macros.ref.mutate
  }
  object Ref {
    def apply[T](value: T)(implicit r: Region): Ref[T] = macro macros.ref.alloc[T]
    def empty[T]: Ref[T]                               = macro macros.ref.empty[T]
  }

  case object EmptyRefException extends Exception

  class struct extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro macros.annotations.struct
  }

  class union extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro macros.annotations.union
  }
}
