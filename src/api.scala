import scala.language.dynamics
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

package regions {
  class Region private[regions](
    private[regions] var node: internal.Node,
    private[regions] var offset: Long
  ) {
    def dispose(): Unit = internal.disposeRegion(this)
  }
  object Region {
    def apply() = internal.allocRegion()
    def apply[T](f: Region => T): T = {
      val r = internal.allocRegion()
      val res = f(r)
      internal.disposeRegion(r)
      res
    }
  }

  final class Ref[A](val loc: Long) extends AnyVal {
    def nonEmpty: Boolean                                      = macro internal.macros.refNonEmpty[A]
    def isEmpty: Boolean                                       = macro internal.macros.refIsEmpty[A]
    def get: A                                                 = macro internal.macros.refGet[A]
    def set(value: A): Unit                                    = macro internal.macros.refSet[A]
    /*
    def getOrElse[B >: A](default: B): B                       = macro internal.macros.refGetOrElse[A]
    def map[B](f: A => B)(implicit r: Region): Ref[B]          = macro internal.macros.refMap[A]
    def fold[B](ifEmpty: B)(f: A => B): B                      = macro internal.macros.refFold[A]
    def flatMap[B](f: A => Ref[B])(implicit r: Region): Ref[B] = macro internal.macros.refFlatMap[A]
    def flatten[B](implicit ev: A <:< Ref[B]): Ref[B]          = macro internal.macros.refFlatten[A]
    def filter(p: A => Boolean): Ref[B]                        = macro internal.macros.refFilter[A]
    def contains[A1 >: A](elem: A1): Boolean                   = macro internal.macros.refContains[A]
    def exists(p: A => Boolean): Boolean                       = macro internal.macros.refExists[A]
    def forall(p: A => Boolean): Boolean                       = macro internal.macros.refForall[A]
    def mutate[B](f: A => B): Unit                             = macro internal.macros.refMutate[A]
    */
  }
  object Ref {
    def apply[T](value: T)(implicit r: Region): Ref[T] = macro internal.macros.refCompanionApply[T]
    def empty[T]: Ref[T]                               = macro internal.macros.refCompanionEmpty[T]
  }

  class struct extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro internal.macros.struct
  }
}
