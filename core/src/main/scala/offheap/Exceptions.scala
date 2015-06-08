package scala.offheap

/** An exception that is thrown when Allocator can not allocate requested memory. */
class OutOfMemoryException(reason: String = "") extends Exception(reason)

/** An exception that is thrown in checked memory mode when a
 *  pointer to inaccassible memory is dereferenced.
 *  (e.g. pointer to closed region)
 */
class InaccessibleMemoryException(reason: String = "") extends Exception(reason)

/** An exception that is thrown when safe `as[T]` cast fails. */
class CastException(reason: String = "") extends Exception(reason)
