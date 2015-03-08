

## Limitations

1. Offheap classes must be defined in statically accessible location.
1. Offheap classes can only inherit from abstract offheap classes and universal traits.
1. Offheap classes may not have fields of on-heap data types.
1. Offheap classes may not have more than one argument lists.
1. Offheap classes may not have an implicit argument list in their constructor.
1. Offheap classes may not have an early initializer.
1. Offheap classes may not have nested classes or objects.
1. Offheap classes may not override equals or hashCode.
1. Offheap classes must have explicitly annotated types on all of their field definitions.
1. Abstract offheap classes can only contain concrete methods and those are final by default.
1. Abstract offheap classes are sealed.
1. Abstract offheap classes can only inherit from other abstract offheap classes.
