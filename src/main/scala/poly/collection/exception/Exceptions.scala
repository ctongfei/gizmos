package poly.collection.exception

class NoSuchElementException extends Exception

class IndexOutOfBoundsException extends Exception

class KeyNotFoundException[K](key: K)
  extends Exception(s"Key $key is not found in the specific collection.")

class NotSupportedException(msg: String)
  extends Exception(msg)

class QueueEmptyException
  extends Exception("Attempted to dequeue an empty queue.")

class EmptyCollectionReductionException
  extends Exception("Cannot reduce an empty collection by a semigroup operation.")

class GoalNotFoundException[S](state: S)
  extends Exception(s"Goal $state not found.")

class VertexNotReachableException[K](i: K, j: K)
  extends Exception(s"A path cannot be found from vertex $i to vertex $j.")
