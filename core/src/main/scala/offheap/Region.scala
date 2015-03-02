package offheap

import scala.language.experimental.{macros => CanMacro}
import offheap.internal.Memory64

trait Region {
  def isOpen: Boolean
  def close(): Unit
  def allocate64(size: Memory64.Size): Memory64.Addr
}
object Region {
  def apply[T](f: Region => T): T = macro internal.macros.Region.apply[T]
  def open(): Region              = macro internal.macros.Region.open
}
