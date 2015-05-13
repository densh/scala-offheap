package test

import org.scalatest.FunSuite
import offheap._

@data class Inner(var v: Int)
@data class Outer(@embed var inner: Inner)

class EmbedSuite extends FunSuite {
  implicit val alloc = Allocator()

  test("inner pointer") {
    val inner = Inner(42)
    val outer = Outer(inner)
    assert(Outer.toAddr(outer) == Inner.toAddr(outer.inner))
  }

  test("modify after copy") {
    val outer = Outer(Inner(42))
    assert(outer.inner.v == 42)
    val outer2 = outer.copy()
    assert(outer2.inner.v == 42)
    outer2.inner.v = 43
    assert(outer.inner.v == 42)
    assert(outer2.inner.v == 43)
  }

  test("assign embedded") {
    val outer = Outer(Inner(42))
    assert(outer.inner.v == 42)
    val inner = Inner(43)
    outer.inner = inner
    assert(inner.v == 43)
    assert(outer.inner.v == 43)
    inner.v = 44
    assert(inner.v == 44)
    assert(outer.inner.v == 43)
  }
}
