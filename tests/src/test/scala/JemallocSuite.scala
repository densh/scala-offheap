package test

import org.scalatest.{FunSuite, ShouldMatchers}

import scala.offheap.jemalloc

class JemallocSuite extends FunSuite with ShouldMatchers {
  test("'allocate' throws IllegalArgumentException for negative size") {
    intercept[IllegalArgumentException] {
      jemalloc.allocate(-1)
    }
  }

  test("'reallocate' throws IllegalArgumentException for negative size") {
    intercept[IllegalArgumentException] {
      val address = jemalloc.allocate(128)
      address should (be > 0l)
      jemalloc.reallocate(address, -1)
    }
  }

  test("'allocate' throws OutOfMemoryError if it fails to allocate") {
    intercept[OutOfMemoryError] {
      jemalloc.allocate(1L << 62)
    }
  }

  test("'reallocate' throws OutOfMemoryError if it fails to reallocate") {
    intercept[OutOfMemoryError] {
      val address = jemalloc.allocate(128)
      address should (be > 0l)
      jemalloc.reallocate(address, 1L << 62)
    }
  }
}
