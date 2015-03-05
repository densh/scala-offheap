package offheap

package object x32 {
  type Addr = Int
  type Size = Int
  type OutOfMemoryException = offheap.OutOfMemoryException
  type InaccessibleRegionException = offheap.InaccessibleRegionException
  protected[offheap] type AtomicFresh = offheap.internal.AtomicFreshInt
}
