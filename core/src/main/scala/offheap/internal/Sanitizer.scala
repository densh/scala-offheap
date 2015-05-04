package offheap
package internal

import java.{lang => jl}
import internal.CheckedHolder.CHECKED
import internal.UnsafeHolder.UNSAFE

object Sanitizer {
  private[this] final val UNPACKED_ID_MASK = 65535L
  private[this] final val ID_MASK = jl.Long.MAX_VALUE << 48
  private[this] final val ADDR_MASK = jl.Long.MAX_VALUE >> 16

  def pack(id: Long, addr: Addr): Addr = (id << 48) | addr
  def unpackId(addr: Addr): Long = (addr >> 48) & UNPACKED_ID_MASK
  def unpackAddr(addr: Addr): Addr = addr & ADDR_MASK

  private[this] final val MAX = 65536
  private[this] val arr = UNSAFE.allocateMemory(MAX)
  private[this] var last: Long = 0
  private[this] var count: Int = 0
  private def advance(last: Long): Long = {
    val inc = last + 1
    if (inc < MAX) inc
    else 0
  }

  def register(): Long = this.synchronized {
    while (UNSAFE.getByte(arr + last) != 0) last = advance(last)
    val res = last
    UNSAFE.putByte(arr + last, 1)
    last = advance(last)
    count += 1
    res
  }
  def unregister(id: Long): Unit = this.synchronized {
    count -= 1
    UNSAFE.putByte(arr + id, 0)
  }

  def validate(addr: Addr): Addr =
    if (CHECKED) {
      if (UNSAFE.getByte(arr + unpackId(addr)) != 1)
        throw new InaccessibleMemoryException
      unpackAddr(addr)
    } else {
      addr
    }
}
