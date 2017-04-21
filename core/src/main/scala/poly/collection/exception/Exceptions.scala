package poly.collection.exception

class InvalidIteratorPositionException
  extends Exception("Attempted to access an iterator at an invalid position.")

class DummyNodeException
  extends Exception("Attempted to access a dummy node.")

class KeyNotFoundException[K](key: K)
  extends Exception(s"Key $key is not found in the specific collection.")

class QueueEmptyException
  extends Exception("Attempted to pop an empty queue.")

class EmptyCollectionReductionException
  extends Exception("Cannot reduce an empty collection by a semigroup.")

class GoalNotFoundException[S](state: S)
  extends Exception(s"Goal $state not found.")

class VertexNotReachableException[K](i: K, j: K)
  extends Exception(s"A path cannot be found from vertex $i to vertex $j.")
