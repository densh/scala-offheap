package offheap
package internal

trait Region {
  def isOpen: Boolean
  def close(): Unit
  def allocate(size: Size): Addr
}
object Region {
  def open(): offheap.Region = new LinkedRegion
}
