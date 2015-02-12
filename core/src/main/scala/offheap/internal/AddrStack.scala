package offheap
package internal

import C._

@struct class AddrStack(arrSize: Long, arr: Ptr[Long], size: Long)
object AddrStack {
  final val GROWTH_FACTOR = 2

  def alloc(startingSize: Long): Ptr[AddrStack] = {
    val ptr = Ptr.alloc[AddrStack](1)
    ptr.arrSize = startingSize
    ptr.arr = Ptr.alloc[Long](startingSize)
    ptr.size = 0L
    ptr
  }

  def isEmpty(stack: Ptr[AddrStack]): Boolean = stack.size == 0

  def nonEmpty(stack: Ptr[AddrStack]): Boolean = stack.size != 0

  def push(stack: Ptr[AddrStack], value: Addr): Unit = {
    if (stack.size >= stack.arrSize) {
      stack.arrSize = (stack.arrSize * GROWTH_FACTOR).toLong
      stack.arr     = stack.arr.resize(stack.arrSize)
    }
    stack.arr(stack.size) = value
    stack.size = stack.size + 1
  }

  def pop(stack: Ptr[AddrStack]): Addr = {
    val newsize = stack.size - 1
    stack.size = newsize
    stack.arr(newsize)
  }

  def merge(stack: Ptr[AddrStack], other: Ptr[AddrStack]): Unit = {
    if (stack.size + other.size >= stack.arrSize) {
      stack.arrSize = ((stack.size + other.size) * GROWTH_FACTOR).toLong
      stack.arr     = stack.arr.resize(stack.arrSize)
    }
    Ptr.copy(other.arr, 0, stack.arr, stack.size, other.size)
    stack.size = stack.size + other.size
  }

  def free(stack: Ptr[AddrStack]): Unit = {
    stack.arr.free
    stack.free
  }
}
