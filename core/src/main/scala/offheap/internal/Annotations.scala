package offheap
package internal
package annot

import scala.annotation.StaticAnnotation

final class offheap(layout: Layout) extends StaticAnnotation
final class struct(layout: Layout) extends StaticAnnotation
final class union(layout: Layout) extends StaticAnnotation
final case class Node(loc: Long, var next: Node)
final case class Layout(fields: (String, Tag[_])*)
final case class Tag[T]()


