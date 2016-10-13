package poly.collection.mut;

/**
 * Represents a graph in which nodes can be added or removed.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait KeyMutableGraph[K, E] extends ValueMutableGraph[K, E] {

  def addNode_!(i: K): Unit
  def removeNode_!(i: K): Unit

  def addArc_!(i: K, j: K, e: E): Unit
  def removeArc_!(i: K, j: K): Unit

}
