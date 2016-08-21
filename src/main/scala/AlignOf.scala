package ptr

trait AlignOf[T] {
  def alignof: Align
}

object AlignOf {
  implicit object AlignOfByte extends AlignOf[Byte] {
    final val alignof: Size = 1
  }

  implicit object AlignOfShort extends AlignOf[Short] {
    final val alignof: Size = 2
  }

  implicit object AlignOfInt extends AlignOf[Int] {
    final val alignof: Size = 4
  }

  implicit object AlignOfLong extends AlignOf[Long] {
    final val alignof: Size = 8
  }

  implicit object AlignOfFloat extends AlignOf[Float] {
    final val alignof: Size = 4
  }

  implicit object AlignOfDouble extends AlignOf[Double] {
    final val alignof: Size = 8
  }
}
