package poly.collection.mut;

/**
 * Represents a graph in which nodes can be added or removed.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait KeyMutableGraph[K, E] extends ValueMutableGraph[K, E] {

  def addNode(i: K): Unit
  def removeNode(i: K): Unit

  def addArc(i: K, j: K, e: E): Unit
  def removeArc(i: K, j: K): Unit

}
