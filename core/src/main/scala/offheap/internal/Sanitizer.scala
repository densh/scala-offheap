package scala.offheap
package internal

import java.{lang => jl}
import internal.SunMisc.UNSAFE

object Sanitizer {
  private[this] final val UNPACKED_ID_MASK = 65535L
  private[this] final val ID_MASK = jl.Long.MAX_VALUE << 48
  private[this] final val ADDR_MASK = jl.Long.MAX_VALUE >> 16

  def pack(id: Long, addr: Addr): Addr = (id << 48) | addr
  def unpackId(addr: Addr): Long = (addr >> 48) & UNPACKED_ID_MASK
  def unpackAddr(addr: Addr): Addr = addr & ADDR_MASK

  private[this] final val MAX = 65536
  private[this] val valid = new java.util.concurrent.atomic.AtomicIntegerArray(MAX)
  private[this] val trans = new java.util.concurrent.atomic.AtomicLong(0L)

  private def truncate(v: Int): Int =
    if (v == 0) 1 else v
  private def advance(prev: Int): Int = {
    val inc = prev + 1
    if (inc < MAX) inc
    else 1
  }

  def register(): Long = {
    var commit = false
    var res = 0
    do {
      val prevtrans = trans.get
      val start = truncate((prevtrans % MAX).toInt)
      res = advance(start)
      while (valid.get(res) == 1 && res != start)
        res = advance(res)
      if (res == start && trans.get == prevtrans)
        throw new IllegalArgumentException(
          s"can't have more than ${MAX-1} regions open in checked memory mode")
      commit = valid.compareAndSet(res, 0, 1)
      trans.incrementAndGet
    } while (!commit)
    res.toLong
  }

  def unregister(id: Long): Unit = {
    valid.compareAndSet(id.toInt, 1, 0)
    trans.incrementAndGet
  }

  def validate(addr: Addr): Addr =
    if (Checked.MEMORY) {
      val id = unpackId(addr).toInt
      if (id != 0 && valid.get(id) != 1) {
        throw new InaccessibleMemoryException
      }
      unpackAddr(addr)
    } else {
      addr
    }
}
