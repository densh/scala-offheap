package offheap
package internal

import scala.annotation.StaticAnnotation

final class Data(layout: Layout) extends StaticAnnotation
final class Enum(layout: Layout) extends StaticAnnotation
final class Parent(tag: Tag[_]) extends StaticAnnotation
final class Children(tags: Tag[_]*) extends StaticAnnotation
final class ClassTag(tag: Any) extends StaticAnnotation
final class ClassTagRange(from: Any, to: Any) extends StaticAnnotation // > from <= to
final class Layout(fields: (String, Tag[_])*)
final class Tag[T]()
