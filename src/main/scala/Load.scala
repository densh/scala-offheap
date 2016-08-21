package ptr

trait Load[T] {
  def load(addr: Addr): T
}

object Load {
  implicit object LoadByte extends Load[Byte] {
    def load(addr: Addr): Byte = unsafe.getByte(addr)
  }

  implicit object LoadShort extends Load[Short] {
    def load(addr: Addr): Short = unsafe.getShort(addr)
  }

  implicit object LoadInt extends Load[Int] {
    def load(addr: Addr): Int = unsafe.getInt(addr)
  }

  implicit object LoadLong extends Load[Long] {
    def load(addr: Addr): Long = unsafe.getLong(addr)
  }

  implicit object LoadFloat extends Load[Float] {
    def load(addr: Addr): Float = unsafe.getFloat(addr)
  }

  implicit object LoadDouble extends Load[Double] {
    def load(addr: Addr): Double = unsafe.getDouble(addr)
  }
}
