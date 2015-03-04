package offheap

import scala.language.experimental.{macros => CanMacro}
import java.nio.ByteBuffer

final case class Ref32(addr: Int, memory: Memory32)

trait Memory32 extends Memory {
  type Addr = Int
  def sizeOfRef: Int                         = 4
  def sizeOf[T]: Int                         = macro internal.macros.Memory.sizeOf32[T]
  def offset(addr: Addr, size: Int): Addr    = addr + size
  def getRef(addr: Addr): Ref32              = Ref32(getInt(addr), this)
  def putRef(addr: Addr, value: Ref32): Unit = putLong(addr, value.addr)
}

class ByteBufferMemory(buffer: ByteBuffer) extends Memory32 {
  def allocate(size: Int): Addr                  = ???
  def getByte(addr: Addr): Byte                  = buffer.get(addr)
  def getChar(addr: Addr): Char                  = buffer.getChar(addr)
  def getShort(addr: Addr): Short                = buffer.getShort(addr)
  def getInt(addr: Addr): Int                    = buffer.getInt(addr)
  def getLong(addr: Addr): Long                  = buffer.getLong(addr)
  def getFloat(addr: Addr): Float                = buffer.getFloat(addr)
  def getDouble(addr: Addr): Double              = buffer.getDouble(addr)
  def putByte(addr: Addr, value: Byte): Unit     = buffer.put(addr, value)
  def putChar(addr: Addr, value: Char): Unit     = buffer.putChar(addr, value)
  def putShort(addr: Addr, value: Short): Unit   = buffer.putShort(addr, value)
  def putInt(addr: Addr, value: Int): Unit       = buffer.putInt(addr, value)
  def putLong(addr: Addr, value: Long): Unit     = buffer.putLong(addr, value)
  def putFloat(addr: Addr, value: Float): Unit   = buffer.putFloat(addr, value)
  def putDouble(addr: Addr, value: Double): Unit = buffer.putDouble(addr, value)
}
