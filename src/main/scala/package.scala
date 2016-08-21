import sun.misc.Unsafe

package object ptr {
  type Addr  = Long
  type Size  = Long
  type Align = Long

  def alloc[T: SizeOf](implicit alloc: Alloc): Ptr[T] =
    new Ptr[T](alloc.alloc(sizeof[T]))

  def alloc[T: SizeOf](n: Size)(implicit alloc: Alloc): Ptr[T] =
    new Ptr[T](alloc.alloc(sizeof[T] * n))

  def sizeof[T: SizeOf]: Size =
    implicitly[SizeOf[T]].sizeof

  def alignof[T: AlignOf]: Size =
    implicitly[AlignOf[T]].alignof

  def load[T: Load](addr: Addr): T =
    implicitly[Load[T]].load(addr)

  def store[T: Store](addr: Addr, value: T): Unit =
    implicitly[Store[T]].store(addr, value)

  private[ptr] final val unsafe: sun.misc.Unsafe = {
    try {
      Unsafe.getUnsafe()
    } catch {
      case _: SecurityException =>
        val f = classOf[Unsafe].getDeclaredField("theUnsafe")
        f.setAccessible(true)
        f.get(null).asInstanceOf[Unsafe]
    }
  }
}
