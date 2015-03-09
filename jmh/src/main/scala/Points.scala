package offheap.test.jmh

import offheap.x64._

class Point1(val a: Long)
class Point2(val a: Long, val b: Long)
class Point4(val a: Long, val b: Long, val c: Long, val d: Long)

@offheap class OffheapPoint1(val a: Long)
@offheap class OffheapPoint2(val a: Long, val b: Long)
@offheap class OffheapPoint4(val a: Long, val b: Long, val c: Long, val d: Long)
