package offheap
package internal

import C._

trait Region { self: offheap.Region =>
  protected[internal] def isOpen: Boolean
  protected[internal] def close(): Unit
  protected[internal] def allocate(size: Size): Addr
}
object Region {
  def open(): offheap.Region = new AddrStackRegion
  def close(r: offheap.Region): Unit = r.close()
  def isOpen(r: offheap.Region): Boolean = r.isOpen
  def allocate(r: offheap.Region, size: Size): Addr = r.allocate(size)
}
