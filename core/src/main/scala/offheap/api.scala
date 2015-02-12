package offheap

import scala.language.dynamics
import scala.language.experimental.{macros => CanMacro}
import scala.annotation.StaticAnnotation

trait Region extends internal.Region
object Region {
  def apply[T](f: Region => T): T =
    macro internal.macros.Region.apply[T]
}

trait Ref extends Any

case object NullRefException extends Exception

case object InaccessiblePageException extends  Exception


