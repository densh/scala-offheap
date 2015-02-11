package offheap
package internal

import C._

class AddrStack(startingSize: Long, growthFactor: Double = 1.5) {
  private var arrSize = startingSize
  private var arr     = Ptr.allocArray[Long](arrSize)
  private var idx     = 0L

  def size: Long        = idx
  def isEmpty: Boolean  = idx == 0
  def nonEmpty: Boolean = idx != 0

  def push(value: Addr): Unit = {
    if (idx >= arrSize) {
      arrSize = (arrSize * growthFactor).toLong
      arr     = arr.resize(arrSize)
    }
    arr(idx) = value
    idx += 1
  }

  def pop: Addr = {
    assert(nonEmpty)
    idx -= 1
    arr(idx)
  }

  def merge(other: AddrStack): Unit = {
    if (idx + other.size >= arrSize) {
      arrSize = ((size + other.size) * growthFactor).toLong
      arr     = arr.resize(arrSize)
    }
    Ptr.copy(other.arr, 0, arr, idx, other.size)
    idx += other.size
  }

  def dispose: Unit = arr.free
}
