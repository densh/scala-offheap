package offheap

import scala.language.experimental.{macros => CanMacro}

trait Region {
  def isOpen: Boolean
  def close(): Unit
  def allocate(size: internal.Size): internal.Addr
}
object Region {
  def apply[T](f: Region => T): T = macro internal.macros.Region.apply[T]
  def open(): Region              = new internal.LinkedRegion
}
