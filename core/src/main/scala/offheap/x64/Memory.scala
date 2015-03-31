package offheap
package x64

import scala.language.experimental.{macros => CanMacro}

trait Memory {
  def allocate(size: Size): Addr
  def copy(from: Addr, to: Addr, size: Size)

  def getChar(addr: Addr): Char
  def getByte(addr: Addr): Byte
  def getShort(addr: Addr): Short
  def getInt(addr: Addr): Int
  def getLong(addr: Addr): Long
  def getFloat(addr: Addr): Float
  def getDouble(addr: Addr): Double
  def getRef(addr: Addr): Ref = {
    val value = getLong(addr)
    if (value == 0L) null else Ref(value, this)
  }

  def putChar(addr: Addr, value: Char): Unit
  def putByte(addr: Addr, value: Byte): Unit
  def putShort(addr: Addr, value: Short): Unit
  def putInt(addr: Addr, value: Int): Unit
  def putLong(addr: Addr, value: Long): Unit
  def putFloat(addr: Addr, value: Float): Unit
  def putDouble(addr: Addr, value: Double): Unit
  def putRef(addr: Addr, value: Ref): Unit =
    if (value != null) putLong(addr, value.addr)
    else putLong(addr, 0L)

  def isNative: Boolean
  def isVirtual: Boolean = !isNative
}
object Memory {
  def sizeof[T]: Size = macro internal.macros.Memory.sizeof_[T]
  def apply(): Memory = new UnsafeMemory()
}
