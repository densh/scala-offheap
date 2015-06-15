package test

import org.scalatest.FunSuite
import scala.offheap.malloc

class MallocSuite extends FunSuite {
  test("out of memory") {
    intercept[OutOfMemoryError] {
      // request 1PB of memory
      malloc.allocate(1024L * 1024L * 1024L * 1024L * 1024L)
    }
  }
}
