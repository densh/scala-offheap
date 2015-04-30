package offheap

import java.{lang => jl}
import internal.CheckedHolder.CHECKED
import internal.UnsafeHolder.UNSAFE

trait Allocator {
  val id: Short = Sanitizer.register()
  def allocate(size: Size): Addr
  def close = Sanitizer.unregister(id)
  def packIfChecked(addr: Addr): Addr = {
    if (CHECKED) Sanitizer.pack(this.id, addr)
    else addr
  }
}
/*
object Sanitizer {
  private[this] val ID_MASK = jl.Long.MAX_VALUE << 48
  private[this] val ADDR_MASK = jl.Long.MAX_VALUE >> 16

  def pack(id: Short, addr: Addr): Addr = (id.toLong << 48) | addr
  def unpackId(addr: Addr): Short = ((addr & ID_MASK) >> 48).toShort
  def unpackAddr(addr: Addr): Addr = addr & ADDR_MASK

  def validate(addr: Addr): Addr = {
    if (CHECKED) {
      //println(s"unpacking $addr into ${unpackId(addr)} and ${unpackAddr(addr)}")
      if (!accessible(unpackId(addr)))
        throw new InaccessibleMemoryException
      unpackAddr(addr)
    } else {
      addr
    }
  }

  private[this] final val MAX = jl.Short.MAX_VALUE
  private[this] val arr = UNSAFE.allocateMemory(MAX)
  private[this] var last: Int = 0
  private[this] var count: Int = 0

  private def advance(last: Int): Int = {
    val inc = last + 1
    if (inc < MAX) inc
    else 0
  }
  def register: Short = this.synchronized {
    while (UNSAFE.getByte(arr + last) != 0) last = advance(last)
    val res = last.toShort
    UNSAFE.putByte(arr + last, 1)
    last = advance(last)
    count += 1
    //println(s"registered $res")
    res
  }
  def unregister(id: Short): Unit = this.synchronized {
    count -= 1
    UNSAFE.putByte(arr + id, 0)
    //println(s"unregistered $id")
  }
  def accessible(id: Short): Boolean = this.synchronized {
    val res = UNSAFE.getByte(arr + id) == 1
    //println(s"is $id accessible? $res")
    res
  }
}
*/
