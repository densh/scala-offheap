package offheap
package internal
package annot

import scala.annotation.StaticAnnotation

final class offheap(layout: Layout) extends StaticAnnotation
final case class Layout(fields: (String, Tag[_])*)
final case class Tag[T]()
