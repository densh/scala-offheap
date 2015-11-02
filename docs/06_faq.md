# FAQ

**Q**: `sun.misc.Unsafe` is planned to be removed in JDK9, how will scala-offheap work without it?

**A**: scala-offheap doesn't use `Unsafe` directly but only through lightweight abstraction layer
of `Memory` and `malloc` modules. If it is going to be removed completely we would just re-implement
those modules using different underlying abstraction. Possible options are JNI and direct byte buffers
whichever is more performant at the time we need to replace `Unsafe`. End-user's code should not
be affected by the change.

---

**Q**: Macro annotations are an experimental feature, what will happen if they are not merged into
mainline Scala and become unsupported?

**A**: In that case we'll re-implement the same functionality (namely `@data` and `@variant` annotations)
using lower-level compiler plugin infrastructure. Considering that macros share reflection APIs with
the compiler most of the code can be re-used.
