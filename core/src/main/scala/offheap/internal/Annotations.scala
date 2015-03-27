package offheap
package internal

import scala.annotation.StaticAnnotation

final class Data extends StaticAnnotation
final class Enum extends StaticAnnotation
final class Parent(tag: Tag[_]) extends StaticAnnotation
final class Children(tags: Tag[_]*) extends StaticAnnotation
final class ClassTag(tag: Any) extends StaticAnnotation
final class ClassTagRange(from: Any, to: Any) extends StaticAnnotation // > from <= to
final class Layout(fields: (String, Tag[_])*) extends StaticAnnotation
final class Tag[T]()
final class ParentExractor(tag: Tag[_], value: Any) extends StaticAnnotation
final class PrimaryExtractor(value: Any) extends StaticAnnotation
final class UniversalExtractor(value: Any) extends StaticAnnotation
final class Unsafe(name: String) extends StaticAnnotation
