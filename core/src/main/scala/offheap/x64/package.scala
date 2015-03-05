package offheap

package object x64 {
  type Addr = Long
  type Size = Long
  type OutOfMemoryException = offheap.OutOfMemoryException
  type InaccessibleRegionException = offheap.InaccessibleRegionException
  protected[offheap] type AtomicFresh = offheap.internal.AtomicFreshLong
}
