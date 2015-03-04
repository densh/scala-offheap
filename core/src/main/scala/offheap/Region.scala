package offheap

trait Region extends Memory {
  def isOpen: Boolean
  def close(): Unit
}

