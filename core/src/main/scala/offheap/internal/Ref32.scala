package offheap
package internal

final case class Ref32(addr: Memory32.Addr, memory: Region32)
