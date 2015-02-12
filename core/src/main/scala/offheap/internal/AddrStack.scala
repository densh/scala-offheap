package offheap
package internal

import C._

object AddrStack {
  final val GROWTH_FACTOR = 2

  @struct class Data(arrSize: Long, arr: Ptr[Long], size: Long)

  type T = Ptr[AddrStack.Data]

  def alloc(startingSize: Long): AddrStack.T = {
    val ptr = Ptr.alloc[AddrStack.Data]
    ptr.arrSize = startingSize
    ptr.arr = Ptr.alloc[Long](arrSize)
    ptr.size = 0L
    ptr
  }

  def isEmpty(stack: AddrStack.T): Boolean = stack.size == 0

  def nonEmpty(stack: AddrStack.T): Boolean = stack.size != 0

  def push(stack: AddrStack.T, value: Addr): Unit = {
    if (stack.size >= stack.arrSize) {
      stack.arrSize = (stack.arrSize * GROWTH_FACTOR).toLong
      stack.arr     = stack.arr.resize(arrSize)
    }
    stack.arr(stack.size) = value
    stack.size += 1
  }

  def pop(stack: AddrStack.T): Addr = {
    assert(nonEmpty(stack))
    stack.size -= 1
    stack.arr(stack.size)
  }

  def merge(stack: AddrStack.T, other: AddrStack.T): Unit = {
    if (stack.size + other.size >= arrSize) {
      stack.arrSize = ((stack.size + other.size) * GROWTH_FACTOR).toLong
      stack.arr     = stack.arr.resize(stack.arrSize)
    }
    Ptr.copy(other.arr, 0, stack.arr, stack.size, other.size)
    stack.size += other.size
  }

  def free(stack: AddrStack.T): Uni = stack.free
}
