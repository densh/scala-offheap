import scala.language.dynamics
import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation
import regions.internal.{runtime, macros}

package regions {
  final class Region private[regions](
    private[regions] var node: runtime.Node,
    private[regions] var offset: Long
  )
  object Region {
    def apply[T](f: Region => T): T = macro internal.macros.Region.alloc[T]
  }

  final class Ref[A, +R <: Region with Singleton](val addr: Long) extends AnyVal {
    def isEmpty: Boolean                                          = macro macros.Ref.isEmpty
    def nonEmpty: Boolean                                         = macro macros.Ref.nonEmpty
    def get: A                                                    = macro macros.Ref.get
    def getOrElse[B >: A](default: => B): B                       = macro macros.Ref.getOrElse
    def set(value: A): Unit                                       = macro macros.Ref.set
    def setOrElse(value: A)(default: => Unit): Unit               = macro macros.Ref.setOrElse
    def contains[A1 >: A](elem: A1): Boolean                      = macro macros.Ref.contains
    def flatten[B, R2](implicit ev: A <:< Ref[B, R2]): Ref[B, R2] = macro macros.Ref.flatten
    def map[B](f: A => B): Ref[B, R]                              = macro macros.Ref.map
    def fold[B](ifEmpty: B)(f: A => B): B                         = macro macros.Ref.fold
    def filter(p: A => Boolean): Ref[A, R]                        = macro macros.Ref.filter
    def exists(p: A => Boolean): Boolean                          = macro macros.Ref.exists
    def forall(p: A => Boolean): Boolean                          = macro macros.Ref.forall
    def mutate[B](f: A => B): Unit                                = macro macros.Ref.mutate
  }
  object Ref {
    def apply[T, R <: Region](value: T)(implicit r: R): Ref[T, r.type]    = macro macros.Ref.alloc[T]
    def empty[T]: Ref[T, Nothing]                                 = macro macros.Ref.empty[T]
  }

  case object EmptyRefException extends Exception

  final class struct extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro macros.Annotations.struct
  }

  final class union extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro macros.Annotations.union
  }
}
