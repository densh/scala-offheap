package offheap.test.jmh

import org.openjdk.jmh.annotations._
import offheap.x64._

@State(Scope.Thread)
class PoolContention {
  implicit val pool: Pool = Pool(Memory())

  @Benchmark
  def contention = {
    val r = Region.open
    r.close
  }
}
