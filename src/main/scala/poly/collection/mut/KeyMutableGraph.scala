package poly.collection.mut

import poly.collection._

/**
 * Represents a graph in which nodes can be added or removed.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait KeyMutableGraph[K, V, E] extends ValueMutableGraph[K, V, E] {

  def addNode(i: K, v: V): Unit
  def removeNode(i: K): Unit

  def addArc(i: K, j: K, e: E): Unit
  def removeArc(i: K, j: K): Unit

}
