package poly.collection.mut

import poly.collection._

/**
 * Represents a graph whose data on nodes / arcs / edges can be mutated.
 * @since 0.1.0
 * @author Tongfei Chen
 */
trait ValueMutableGraph[K, E] extends Graph[K, E] {

  /** Sets the value associated with the arc/edge specified by the given two keys. */
  def update(i: K, j: K, e: E): Unit

}
