package test

import org.scalatest.FunSuite
import scala.offheap.Memory

class MemorySuite extends FunSuite {
  test("isAvailable") {
    assert(Memory.isAvailable)
  }
}
