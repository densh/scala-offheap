package offheap

class OutOfMemoryException(reason: String = "") extends Exception(reason)

class InaccessibleMemoryException(reason: String = "") extends Exception(reason)

class CastException(reason: String = "") extends Exception(reason)
