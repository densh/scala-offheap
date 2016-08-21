package ptr

trait Store[T] {
  def store(addr: Addr, value: T): Unit
}

object Store {
  implicit object StoreByte extends Store[Byte] {
    def store(addr: Addr, value: Byte): Unit = unsafe.putByte(addr, value)
  }

  implicit object StoreShort extends Store[Short] {
    def store(addr: Addr, value: Short): Unit = unsafe.putShort(addr, value)
  }

  implicit object StoreInt extends Store[Int] {
    def store(addr: Addr, value: Int): Unit = unsafe.putInt(addr, value)
  }

  implicit object StoreLong extends Store[Long] {
    def store(addr: Addr, value: Long): Unit = unsafe.putLong(addr, value)
  }

  implicit object StoreFloat extends Store[Float] {
    def store(addr: Addr, value: Float): Unit = unsafe.putFloat(addr, value)
  }

  implicit object StoreDouble extends Store[Double] {
    def store(addr: Addr, value: Double): Unit = unsafe.putDouble(addr, value)
  }
}
