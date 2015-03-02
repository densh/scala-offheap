package offheap
package internal

trait Memory64 {
  type Addr = Long
  type Size = Long
  def allocate(size: Size): Addr
  def getChar(addr: Addr): Char
  def getByte(addr: Addr): Byte
  def getShort(addr: Addr): Short
  def getInt(addr: Addr): Int
  def getLong(addr: Addr): Long
  def getFloat(addr: Addr): Float
  def getDouble(addr: Addr): Double
  def putChar(addr: Addr, value: Char): Unit
  def putByte(addr: Addr, value: Byte): Unit
  def putShort(addr: Addr, value: Short): Unit
  def putInt(addr: Addr, value: Int): Unit
  def putLong(addr: Addr, value: Long): Unit
  def putFloat(addr: Addr, value: Float): Unit
  def putDouble(addr: Addr, value: Double): Unit
}
