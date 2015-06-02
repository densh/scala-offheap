package offheap.test.jmh

import org.openjdk.jmh.annotations._
import offheap._

@State(Scope.Thread)
class PoolContention {
  implicit val pool: Pool = Pool()

  @Benchmark
  def contention = {
    val r = Region.open
    r.close
  }
}
