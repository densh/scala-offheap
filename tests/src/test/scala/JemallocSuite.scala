package test

import org.scalatest.{FunSuite, ShouldMatchers}

import scala.offheap.internal.JemallocWrapper
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

  // TODO AR is there a better way to test this than catching OutOfMemoryError vs IllegalArgumentException?
  test("Allocation requests must be valid for OS word size") {
    if (JemallocWrapper.Is32BitWordSize)
      run32BitAllocationTest()
    else
      run64BitWordSizeTest()
  }

  test("Reallocation requests must be valid for OS word size") {
    if (JemallocWrapper.Is32BitWordSize)
      run32BitAllocationTest()
    else
      run64BitWordSizeTest()
  }

  private def run32BitAllocationTest(): Unit = {
    println("*** Running under presumed 32 bit word size")

    intercept[IllegalArgumentException] {
      jemalloc.allocate(JemallocWrapper.Max32BitAllocationRequestSize + 1)
    }

    var allocateThrewOutOfMemoryOrSucceededButDidNotThrowIllegalArgumentException = false
    try {
      jemalloc.allocate(JemallocWrapper.Max32BitAllocationRequestSize)
      allocateThrewOutOfMemoryOrSucceededButDidNotThrowIllegalArgumentException = true
    } catch {
      case _: OutOfMemoryError => allocateThrewOutOfMemoryOrSucceededButDidNotThrowIllegalArgumentException = true
    }

    allocateThrewOutOfMemoryOrSucceededButDidNotThrowIllegalArgumentException should be(true)
  }

  private def run64BitWordSizeTest(): Unit = {
    println("*** Running under presumed 64 bit word size")

    var allocateThrewOutOfMemoryOrSucceededButDidNotThrowIllegalArgumentException = false
    try {
      jemalloc.allocate(Long.MaxValue)
      allocateThrewOutOfMemoryOrSucceededButDidNotThrowIllegalArgumentException = true
    } catch {
      case _: OutOfMemoryError => allocateThrewOutOfMemoryOrSucceededButDidNotThrowIllegalArgumentException = true
    }

    allocateThrewOutOfMemoryOrSucceededButDidNotThrowIllegalArgumentException should be(true)
  }
}
