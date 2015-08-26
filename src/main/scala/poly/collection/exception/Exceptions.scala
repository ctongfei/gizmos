package poly.collection.exception

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class NoSuchElementException extends Exception

class IndexOutOfBoundsException extends Exception

class KeyNotFoundException[K](key: K)
  extends Exception(s"Key $key is not found in the specific collection.")

class NotSupportedException(msg: String)
  extends Exception(msg)

class EnumeratorPositionException
  extends Exception("The enumerator points to an invalid position.")

class QueueEmptyException
  extends Exception("Attempted to dequeue an empty queue.")

class IncompatibleOrderException
  extends Exception("Attempted to merge two sorted sequence based on different orders.")

class GoalNotFoundException[S](state: S)
  extends Exception(s"Goal $state not found.")

class VertexNotReachableException[K](i: K, j: K)
  extends Exception(s"A path cannot be found from vertex $i to vertex $j.")