package test

import org.scalatest.FunSuite
import offheap._
import E1._, E2._

@enum class E1
object E1 {
  @data class D1
  @enum object E2 {
    @data class D2
  }
  @data class D3
}
@data class D4
class C

object Response {
  @data class Success(value: Int)
  @data class Fail
}
import Response._
@enum class Response {
  def map(f: Int => Int)(implicit alloc: Allocator): Response = this match {
    case Success(value) => Success(f(value))
    case Fail()         => this
  }
  def value: Int = this match {
    case Success(value) => value
    case _              => throw new Exception("fail")
  }
}


class EnumSuite extends FunSuite {
  implicit val alloc = malloc

  test("D1 is D1") { assert(D1().is[D1]) }
  test("D2 is D2") { assert(D2().is[D2]) }
  test("D3 is D3") { assert(D3().is[D3]) }
  test("D4 is D4") { assert(D4().is[D4]) }

  test("D1 is E1")     { assert( D1().is[E1]) }
  test("D2 is E1")     { assert( D2().is[E1]) }
  test("D3 is E1")     { assert( D3().is[E1]) }
  test("D4 is not E1") { assert(!D4().is[E1]) }

  test("D1 is not E2") { assert(!D1().is[E2]) }
  test("D2 is E2")     { assert( D2().is[E2]) }
  test("D3 is not E2") { assert(!D3().is[E2]) }
  test("D4 is not E2") { assert(!D4().is[E2]) }

  test("D1 as E1 is D1") { assert(D1().as[E1].is[D1]) }
  test("D2 as E1 is D2") { assert(D2().as[E1].is[D2]) }
  test("D3 as E1 is D3") { assert(D3().as[E1].is[D3]) }

  test("D2 as E2 is E1") { assert(D2().as[E2].is[E1]) }
  test("D2 as E2 is D2") { assert(D2().as[E2].is[D2]) }

  test("D1 as E1 as D1 == D1") { val d = D1(); assert(d.as[E1].as[D1] == d) }
  test("D2 as E1 as D1 == D2") { val d = D2(); assert(d.as[E1].as[D2] == d) }
  test("D3 as E1 as D1 == D3") { val d = D3(); assert(d.as[E1].as[D3] == d) }

  test("D1 as E2 throws") { intercept[CastException] { D1().as[E2] } }
  test("D3 as E2 throws") { intercept[CastException] { D3().as[E2] } }
  test("D4 as E1 throws") { intercept[CastException] { D4().as[E1] } }
  test("D4 as E2 throws") { intercept[CastException] { D4().as[E2] } }

  test("D1 is not C") { assert(!D1().is[C]) }
  test("D2 is not C") { assert(!D1().is[C]) }
  test("D3 is not C") { assert(!D1().is[C]) }
  test("D4 is not C") { assert(!D1().is[C]) }

  test("D1 as C throws") { intercept[CastException] { D1().as[C] } }
  test("D2 as C throws") { intercept[CastException] { D2().as[C] } }
  test("D3 as C throws") { intercept[CastException] { D3().as[C] } }
  test("D4 as C throws") { intercept[CastException] { D4().as[C] } }

  test("D1 toString") { assert(D1().toString == "E1.D1()"   ) }
  test("D2 toString") { assert(D2().toString == "E1.E2.D2()") }
  test("D3 toString") { assert(D3().toString == "E1.D3()"   ) }
  test("D4 toString") { assert(D4().toString == "D4()"      ) }

  test("coerce D1 to E1") { val e1: E1 = D1()        }
  test("coerce D2 to E1") { val e1: E1 = D2()        }
  test("coerce D3 to E1") { val e1: E1 = D3()        }
  test("coerce D2 to E2") { val e1: E2 = D2()        }
  test("coerce E2 to E1") { val e1: E1 = D2().as[E2] }

  test("match E1 as D1") { val D1() = D1().as[E1] }
  test("match E1 as D2") { val D2() = D2().as[E1] }
  test("match E1 as D3") { val D3() = D3().as[E1] }
  test("match E2 as D2") { val D2() = D2().as[E2] }

  test("match D1 as D2 fails") { intercept[MatchError] { val D1() = D2()  } }
  test("match C as D1 fails")  { intercept[MatchError] { val D1() = new C } }

  test("map response") {
    val resp: Response = Success(42)
    val resp2 = resp.map(_ - 2)
    assert(resp2.value == 40)
  }

  test("get on success") {
    val succ = Success(42)
    assert(succ.value == 42)
  }
}
