package offheap
package internal

import java.{lang => jl}
import internal.Memory.UNSAFE

object Sanitizer {
  private[this] final val UNPACKED_ID_MASK = 65535L
  private[this] final val ID_MASK = jl.Long.MAX_VALUE << 48
  private[this] final val ADDR_MASK = jl.Long.MAX_VALUE >> 16

  def pack(id: Long, addr: Addr): Addr = (id << 48) | addr
  def unpackId(addr: Addr): Long = (addr >> 48) & UNPACKED_ID_MASK
  def unpackAddr(addr: Addr): Addr = addr & ADDR_MASK

  private[this] final val MAX = 65536
  private[this] val valid = new scala.Array[Boolean](MAX)
  private[this] var last  = 1

  private def advance(last: Int): Int = {
    val inc = last + 1
    if (inc < MAX) inc
    else 1
  }

  def register(): Long = this.synchronized {
    val prev = last
    var res = advance(last)
    while (valid(res) && res != prev) {
      res = advance(res)
    }
    if (res == prev)
      throw new IllegalArgumentException(
        s"can't open more than ${MAX-1} regions in checked memory mode")
    valid(res) = true
    last = res
    res.toLong
  }

  def unregister(id: Long): Unit = this.synchronized {
    valid(id.toInt) = false
  }

  def validate(addr: Addr): Addr =
    if (Checked.MEMORY) {
      val id = unpackId(addr).toInt
      if (id != 0 && !valid(id)) {
        throw new InaccessibleMemoryException
      }
      unpackAddr(addr)
    } else {
      addr
    }
}
