import scala.language.dynamics
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import regions.internal.runtime

package regions {
  class Region private[regions](
    private[regions] var node: runtime.Node,
    private[regions] var offset: Long
  )
  object Region {
    def apply[T](f: Region => T): T = macro internal.macros.region.alloc[T]
  }

  final class Ref[A](val addr: Long) extends AnyVal {
    def nonEmpty: Boolean                                      = macro internal.macros.ref.nonEmpty[A]
    def isEmpty: Boolean                                       = macro internal.macros.ref.isEmpty[A]
    def get: A                                                 = macro internal.macros.ref.get[A]
    def getOrElse[B >: A](default: => B): B                    = macro internal.macros.ref.getOrElse[A]
    def set(value: A): Unit                                    = macro internal.macros.ref.set[A]
    def setOrElse(value: A)(default: => Unit): Unit            = macro internal.macros.ref.setOrElse[A]
    def contains[A1 >: A](elem: A1): Boolean                   = macro internal.macros.ref.contains[A]
    def flatten[B](implicit ev: A <:< Ref[B]): Ref[B]          = macro internal.macros.ref.flatten[A]
    def map[B](f: A => B)(implicit r: Region): Ref[B]          = macro internal.macros.ref.map[A]
    def fold[B](ifEmpty: B)(f: A => B): B                      = macro internal.macros.ref.fold[A]
    /*
    def flatMap[B](f: A => Ref[B])(implicit r: Region): Ref[B] = macro internal.macros.ref.flatMap[A]
    def filter(p: A => Boolean): Ref[B]                        = macro internal.macros.ref.filter[A]
    def exists(p: A => Boolean): Boolean                       = macro internal.macros.ref.exists[A]
    def forall(p: A => Boolean): Boolean                       = macro internal.macros.ref.forall[A]
    def mutate[B](f: A => B): Unit                             = macro internal.macros.ref.mutate[A]
    */
  }
  object Ref {
    def apply[T](value: T)(implicit r: Region): Ref[T] = macro internal.macros.ref.alloc[T]
    def empty[T]: Ref[T]                               = macro internal.macros.ref.empty[T]
  }

  case object EmptyRefException extends Exception

  class struct extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro internal.macros.annotations.struct
  }

  class union extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro internal.macros.annotations.union
  }
}
