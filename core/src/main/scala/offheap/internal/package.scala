package scala.offheap

package object internal {
  def pad(addr: Long, alignment: Size): Long = {
    val alignmentMask = alignment - 1
    val padding =
      if ((addr & alignmentMask) == 0) 0
      else alignment - (addr & alignmentMask)
    addr + padding
  }
}
