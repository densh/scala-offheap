package offheap

import scala.language.experimental.{macros => CanMacro}
import offheap.internal.Unsafer.unsafe

final case class Ref64(addr: Long, memory: Memory64)

trait Memory64 extends Memory {
  type Addr = Long
  def sizeOfRef: Int                         = 8
  def sizeOf[T]: Int                         = macro internal.macros.Memory.sizeOf[T]
  def offset(addr: Addr, size: Int): Addr    = addr + size
  def getRef(addr: Addr): Ref64              = Ref64(getLong(addr), this)
  def putRef(addr: Addr, value: Ref64): Unit = putLong(addr, value.addr)
}

object UnsafeMemory extends Memory64 {
  assert(unsafe.addressSize() == 8,
    "unsafe memory is only supported 64-bit systems")

  def allocate(size: Int): Addr                  = unsafe.allocateMemory(size)
  def getChar(addr: Addr): Char                  = unsafe.getChar(addr)
  def getByte(addr: Addr): Byte                  = unsafe.getByte(addr)
  def getShort(addr: Addr): Short                = unsafe.getShort(addr)
  def getInt(addr: Addr): Int                    = unsafe.getInt(addr)
  def getLong(addr: Addr): Long                  = unsafe.getLong(addr)
  def getFloat(addr: Addr): Float                = unsafe.getFloat(addr)
  def getDouble(addr: Addr): Double              = unsafe.getDouble(addr)
  def putChar(addr: Addr, value: Char): Unit     = unsafe.putChar(addr, value)
  def putByte(addr: Addr, value: Byte): Unit     = unsafe.putByte(addr, value)
  def putShort(addr: Addr, value: Short): Unit   = unsafe.putShort(addr, value)
  def putInt(addr: Addr, value: Int): Unit       = unsafe.putInt(addr, value)
  def putLong(addr: Addr, value: Long): Unit     = unsafe.putLong(addr, value)
  def putFloat(addr: Addr, value: Float): Unit   = unsafe.putFloat(addr, value)
  def putDouble(addr: Addr, value: Double): Unit = unsafe.putDouble(addr, value)
}
