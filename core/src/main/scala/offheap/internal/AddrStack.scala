package offheap
package internal

class AddrStack(startingSize: Int, growthFactor: Double = 1.5) {
  private var arr = new Array[Addr](startingSize)
  private var idx = 0

  def size: Int         = idx
  def isEmpty: Boolean  = idx == 0
  def nonEmpty: Boolean = idx != 0

  def push(value: Addr): Unit =
    if (idx < arr.length) {
      arr(idx) = value
      idx += 1
    } else {
      val newarr = new Array[Long]((arr.size * growthFactor).toInt)
      System.arraycopy(arr, 0, newarr, 0, arr.size)
      arr = newarr
      push(value)
    }

  def pop: Addr = {
    assert(nonEmpty)
    idx -= 1
    arr(idx)
  }

  def merge(other: AddrStack): Unit = {
    if (idx + other.size < arr.length) {
      System.arraycopy(other.arr, 0, arr, idx, other.size)
    } else {
      val newarr = new Array[Long](((size + other.size) * growthFactor).toInt)
      System.arraycopy(arr, 0, newarr, 0, arr.size)
      System.arraycopy(other.arr, 0, newarr, arr.size, other.size)
      arr = newarr
    }
    idx += other.size
  }
}
