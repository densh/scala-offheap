package offheap.test.jmh

import offheap._

class Point1(val _1: Long)
class Point2(val _1: Long, val _2: Long)
class Point4(val _1: Long, val _2: Long, val _3: Long, val _4: Long)

@offheap class OffheapPoint1(val _1: Long)
@offheap class OffheapPoint2(val _1: Long, val _2: Long)
@offheap class OffheapPoint4(val _1: Long, val _2: Long, val _3: Long, val _4: Long)
