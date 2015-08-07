package poly.collection.exception

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class NoSuchElementException extends Exception

class IndexOutOfBoundsException extends Exception

class KeyNotFoundException extends Exception

class NotSupportedException(msg: String) extends Exception

class EnumeratorPositionException
  extends Exception("The enumerator points to an invalid position.")

class QueueEmptyException
  extends Exception("Attempted to dequeue an empty queue.")

class IncompatibleOrderException
  extends Exception("Attempted to merge two sorted sequence based on different orders.")

class GoalNotFoundException[S](s: S)
  extends Exception("Goal not found.")

class VertexNotReachableException[K](i: K, j: K)
  extends Exception(s"A path cannot be found from vertex $i to vertex $j.")