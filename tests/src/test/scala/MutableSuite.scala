package test

import org.scalatest.FunSuite
import offheap._

@offheap class C1(var x: Int)
//@offheap class C2 { var x = 2 }

class MutableSuite extends FunSuite {
  implicit val r = Region.open(internal.Pool64(internal.UnsafeMemory64))
  protected override def finalize = r.close

  test("mutable constructor argument") {
    val c1 = C1(10)
    assert(c1.x == 10)
    c1.x = 20
    assert(c1.x == 20)
  }

  /*test("mutable body val") {
    val c2 = new C2
    assert(c2.x == 2)
    c2.x = 3
    assert(c2.x == 3)
  }*/
}
