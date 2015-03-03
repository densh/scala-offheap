package offheap

import scala.language.experimental.{macros => CanMacro}
import offheap.internal._

trait Region {
  def isOpen: Boolean
  def close(): Unit
  def allocate32(size: Memory32.Size): Ref32 =
    throw new UnsupportedOperationException
  def allocate64(size: Memory64.Size): Ref64 =
    throw new UnsupportedOperationException
}
object Region {
  def apply[T](f: Region => T): T = macro macros.Region.apply[T]
  def open(): Region              = macro macros.Region.open
}
