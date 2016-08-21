package ptr

trait Alloc {
  def alloc(size: Size): Addr
  def realloc(addr: Addr, size: Size): Addr
  def free(addr: Addr): Unit
}

object Alloc {
  implicit object System extends Alloc {
    def alloc(size: Size): Addr = unsafe.allocateMemory(size)

    def realloc(addr: Addr, size: Size) = unsafe.reallocateMemory(addr, size)

    def free(addr: Addr): Unit = unsafe.freeMemory(addr)
  }
}
