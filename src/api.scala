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

  final class Ref[A](val addr: Long) extends AnyVal {
    def isEmpty: Boolean                               = macro macros.Ref.isEmpty
    def nonEmpty: Boolean                              = macro macros.Ref.nonEmpty
    def get: A                                         = macro macros.Ref.get
    def getOrElse[B >: A](default: => B): B            = macro macros.Ref.getOrElse
    def set(value: A): Unit                            = macro macros.Ref.set
    def setOrElse(value: A)(default: => Unit): Unit    = macro macros.Ref.setOrElse
    def contains[A1 >: A](elem: A1): Boolean           = macro macros.Ref.contains
    def flatten[B](implicit ev: A <:< Ref[B]): Ref[B]  = macro macros.Ref.flatten
    def map[B](f: A => B)(implicit r: Region): Ref[B]  = macro macros.Ref.map
    def fold[B](ifEmpty: B)(f: A => B): B              = macro macros.Ref.fold
    def filter(p: A => Boolean): Ref[A]                = macro macros.Ref.filter
    def exists(p: A => Boolean): Boolean               = macro macros.Ref.exists
    def forall(p: A => Boolean): Boolean               = macro macros.Ref.forall
    def mutate[B](f: A => B): Unit                     = macro macros.Ref.mutate
  }
  object Ref {
    def empty[T]: Ref[T]                               = macro macros.Ref.empty[T]
    def apply[T](value: T)(implicit r: Region): Ref[T] = macro macros.Ref.alloc[T]
  }

  /*
  final class FatRef[A](val addr: Long) extends AnyVal {
    def isEmpty: Boolean                                    = macro macros.FatRef.isEmpty
    def nonEmpty: Boolean                                   = macro macros.FatRef.nonEmpty
    def apply(index: Long): A                               = macro macros.FatRef.apply
    def update(index: Long, value: A): Unit                 = macro macros.FatRef.update
  }
  object FatRef {
    def empty[T]: Ref[T]                                            = macro macros.FatRef.empty[T]
    def apply[T](values: T*)(implicit r: Region): FatRef[T]         = macro macros.FatRef.alloc[T]
    def fill[T](n: Long)(elem: => T)(implicit r: Region): FatRef[T] = macro macros.FatRef.fill[T]
  }
  */

  case object EmptyRefException extends Exception

  final class struct extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro macros.Annotations.struct
  }

  final class union extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro macros.Annotations.union
  }
}
