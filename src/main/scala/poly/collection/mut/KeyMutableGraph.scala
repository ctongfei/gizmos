package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait KeyMutableGraph[K, V, E] extends DataMutableGraph[K, V, E] {

  def addVertex(i: K, v: V): Unit
  def removeVertex(i: K): Unit

  def addEdge(i: K, j: K, e: E): Unit
  def removeEdge(i: K, j: K): Unit

}
