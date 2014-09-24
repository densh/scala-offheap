object Test extends App {
  regions.withRegion { r =>
    val p = r.alloc(Array(1, 2, 3))
    p(1) = 9
    println(p().toList)
  }
}
