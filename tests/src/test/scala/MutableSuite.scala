package test

import org.scalatest.FunSuite
import offheap._

@data class C1(var x: Int)

@data class C2 { var x: Int = 2 }

@data class C3 { C3.x = 10 }
object C3 { var x = 0 }

@data class C4 {
  C4.before = x
  val x: Int = 42
  C4.after = x
}
object C4 {
  var before = 0
  var after = 0
}

@data class C5 {
  var x: Long = _
}

class MutableSuite extends FunSuite {
  implicit val alloc = malloc

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
    assert(C3.x == 0)
    val c3 = C3()
    assert(C3.x == 10)
  }

  test("default value") {
    val c4 = C4()
    assert(C4.before == 0)
    assert(C4.after == 42)
  }

  test("default init var") {
    assert(C5().x == 0L)
  }
}
