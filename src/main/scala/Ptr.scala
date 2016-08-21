package ptr

final class Ptr[T](addr: Addr) {
  def unary_!(implicit load: Load[T]): T =
    ptr.load(addr)

  def `unary_!_=`(value: T)(implicit store: Store[T]): Unit =
    ptr.store(addr, value)

  def apply(i: Size)(implicit sizeof: SizeOf[T], access: Load[T]): T =
    (this + i).unary_!

  def update(i: Size, value: T)(implicit sizeof: SizeOf[T],
                                access: Store[T]): Unit =
    (this + i).`unary_!_=`(value)

  def +(i: Size)(implicit sizeof: SizeOf[T]): Ptr[T] =
    new Ptr(addr + i * ptr.sizeof[T])

  def -(i: Size)(implicit sizeof: SizeOf[T]): Ptr[T] =
    new Ptr(addr - i * ptr.sizeof[T])
}
