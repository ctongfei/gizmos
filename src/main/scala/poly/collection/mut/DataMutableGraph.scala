package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait DataMutableGraph[K, V, E] extends Graph[K, V, E] {

  def update(i: K, v: V): Unit
  def update(i: K, j: K, e: E): Unit

}
