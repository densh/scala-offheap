package jmh

import org.openjdk.jmh.annotations._
import scala.offheap._

@State(Scope.Thread)
class PoolContention {
  implicit val props = Region.Props(Pool())

  @Benchmark
  def contention = {
    val r = Region.open
    r.close
  }
}
