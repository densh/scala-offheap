package offheap

case object NullRefException extends Exception

case object InaccessibleRegionException extends Exception

case object InaccessiblePageException extends Exception

case object OutOfMemoryException extends Exception
