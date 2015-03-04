package offheap

trait Pool
object Pool {
  //def apply(memory: Memory32): Pool32 = Pool32(memory)
  def apply(memory: Memory64): Pool64 = Pool64(memory)
}


