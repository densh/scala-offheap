package offheap
package x64

final case class Ref(addr: Addr, memory: Memory) {
  if (addr == 0) throw new IllegalArgumentException("Ref's addr can not be 0L")
}


