package offheap.test.jmh

import org.openjdk.jmh.annotations._
import offheap._

@State(Scope.Thread)
class PoolContention {
  implicit val props = Region.Props.pool()

  @Benchmark
  def contention = {
    val r = Region.open
    r.close
  }
}
