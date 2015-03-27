package test

import org.scalatest.FunSuite
import offheap._

object UnsafeDummy {
  @unsafe def m(x: Int) = x + 2
  @unsafe def n(x: Int) = m(x) * 2
}
import UnsafeDummy._

class UnsafeSuite extends FunSuite {
  test("can call unsafe method in unsafe scope") {
    unsafe {
      assert(m(10) == 12)
    }
  }

  test("unsafe method can call another unsafe method") {
    unsafe {
      assert(n(10) == 24)
    }
  }
}
