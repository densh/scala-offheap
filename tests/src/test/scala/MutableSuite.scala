package test

import org.scalatest.FunSuite
import offheap._, x64._

@offheap class C1(var x: Int)

@offheap class C2 { var x: Int = 2 }

@offheap class C3 { C3.x = 10 }
object C3 { var x = 0 }

class MutableSuite extends FunSuite {
  implicit val r = Region.open(Pool(UnsafeMemory))
  protected override def finalize = r.close

  test("mutable constructor argument") {
    val c1 = C1(10)
    assert(c1.x == 10)
    c1.x = 20
    assert(c1.x == 20)
  }

  test("mutable body val") {
    val c2 = C2()
    assert(c2.x == 2)
    c2.x = 3
    assert(c2.x == 3)
  }

  test("side-effecting body") {
    val c3 = C3()
    assert(C3.x == 0)
  }
}
