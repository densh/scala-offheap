package ptr

trait SizeOf[T] {
  def sizeof: Size
}

object SizeOf {
  implicit object SizeOfByte extends SizeOf[Byte] {
    final val sizeof: Size = 1
  }

  implicit object SizeOfShort extends SizeOf[Short] {
    final val sizeof: Size = 2
  }

  implicit object SizeOfInt extends SizeOf[Int] {
    final val sizeof: Size = 4
  }

  implicit object SizeOfLong extends SizeOf[Long] {
    final val sizeof: Size = 8
  }

  implicit object SizeOfFloat extends SizeOf[Float] {
    final val sizeof: Size = 4
  }

  implicit object SizeOfDouble extends SizeOf[Double] {
    final val sizeof: Size = 8
  }
}
