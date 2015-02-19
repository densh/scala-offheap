package offheap

import scala.language.experimental.{macros => CanMacro}

trait Region extends internal.Region
object Region {
  def apply[T](f: Region => T): T =
    macro internal.macros.Region.apply[T]
}
