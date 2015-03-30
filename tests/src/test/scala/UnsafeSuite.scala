package test

import org.scalatest.FunSuite
import offheap._

object UnsafeModule {
  @unsafe def m(x: Int) = x + 2
  @unsafe def n(x: Int) = m(x) * 2

  @unsafe def overload(x: Int) = -x
  @unsafe def overload(x: Boolean) = !x
}

trait UnsafeParent {
  @unsafe def foo: Int
  @unsafe def bar = foo + 2
}
class UnsafeChild extends UnsafeParent {
  @unsafe def foo = 2
  @unsafe override def bar = 3
}

trait UnsafeTrait1 {
  @unsafe def foo = 3
}
trait UnsafeTrait2 extends UnsafeTrait1 {
  @unsafe override def foo = super.foo * 5
}

class UnsafeSuite extends FunSuite {
  import UnsafeModule._

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

  test("can call overloaded unsafe method in unsafe scope") {
    unsafe {
      assert(overload(3) == -3)
      assert(overload(false) == true)
    }
  }

  test("can call abstract unsafe method in unsafe scope") {
    val p: UnsafeParent = new UnsafeChild
    unsafe {
      assert(p.foo == 2)
      assert(p.bar == 3)
    }
  }

  test("mixing composition of unsafe methods") {
    val composition = new UnsafeTrait1 with UnsafeTrait2 {}
    unsafe {
      assert(composition.foo == 15)
    }
  }
}
