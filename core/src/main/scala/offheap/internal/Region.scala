package offheap
package internal

trait Region { self: offheap.Region =>
  def isOpen: Boolean
  def close(): Unit
  def allocate(size: Size): Addr
}
object Region {
  def open(): offheap.Region = new CASLinkedRegion
  def close(r: offheap.Region): Unit = r.close()
  def isOpen(r: offheap.Region): Boolean = r.isOpen
  def allocate(r: offheap.Region, size: Size): Addr = r.allocate(size)
}
