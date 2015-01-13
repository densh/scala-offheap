package regions

import scala.language.dynamics
import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation
import regions.internal.{runtime, macros}

final class Region private[regions](
  private[regions] var node: runtime.Node,
  private[regions] var offset: Long
)
object Region {
  def apply[T](f: Region => T): T = macro internal.macros.Region.alloc[T]
}

final class Ref[+A](val addr: Long) extends AnyVal {
  def isEmpty: Boolean                               = macro macros.Ref.isEmpty
  def nonEmpty: Boolean                              = macro macros.Ref.nonEmpty
  def get: A                                         = macro macros.Ref.get
  def get[B](f: A => B): B                           = macro macros.Ref.getF
  def getOrElse[B >: A](default: => B): B            = macro macros.Ref.getOrElse
  def getOrElse[B >: A](f: A => B, default: => B): B = macro macros.Ref.getFOrElse
  def contains[A1 >: A](elem: A1): Boolean           = macro macros.Ref.contains
  def map[B](f: A => B)(implicit r: Region): Ref[B]  = macro macros.Ref.map
  def fold[B](ifEmpty: B)(f: A => B): B              = macro macros.Ref.fold
  def filter(p: A => Boolean): Ref[A]                = macro macros.Ref.filter
  def exists(p: A => Boolean): Boolean               = macro macros.Ref.exists
  def forall(p: A => Boolean): Boolean               = macro macros.Ref.forall
  def flatten[B](implicit ev: A <:< Ref[B]): Ref[B]  = macro macros.Ref.flatten
}
object Ref {
  def apply[T](value: T)(implicit r: Region): Ref[T] = macro macros.Ref.alloc[T]
  def empty[T]: Ref[T]                               = macro macros.Ref.empty[T]
}

// TODO: class ArrayRef[A](val addr: Long) extends AnyVal
// TODO: object ArrayRef

case object EmptyRefException extends Exception

final class offheap extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro macros.Annotations.offheap
}
