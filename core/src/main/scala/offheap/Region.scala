package offheap

import scala.language.experimental.{macros => CanMacro}
import offheap.internal._

trait Region {
  def isOpen: Boolean
  def close(): Unit

  // semi-private api
  def allocate32(addr: Memory32.Size): Memory32.Addr
  def allocate64(addr: Memory64.Size): Memory64.Addr
  def allocateRef32(addr: Memory32.Size): Ref32
  def allocateRef64(addr: Memory64.Size): Ref64
}
object Region {
  def open(): Region              = macro macros.Region.open
  def apply[T](f: Region => T): T = macro macros.Region.apply[T]
}
