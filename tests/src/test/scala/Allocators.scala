package test

import offheap.Allocator
import offheap.malloc
import offheap.jemalloc

trait HasAllocator {
  def allocator(): Allocator
}

trait DefaultAllocator extends HasAllocator {
  def allocator(): Allocator = malloc
}

trait Jemalloc extends HasAllocator {
  def allocator(): Allocator = jemalloc
}
