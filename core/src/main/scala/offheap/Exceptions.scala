package offheap

class OutOfMemoryException(reason: String = "") extends Exception(reason)

class InaccessibleRegionException(reason: String = "") extends Exception(reason)

class CastException(reason: String = "") extends Exception(reason)
