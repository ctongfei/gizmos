package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait KeyMutableMap[K, V] extends DataMutableMap[K, V] {

  def add(x: K, y: V): Unit

  def add(xy: (K, V)): Unit = add(xy._1, xy._2)

  def remove(x: K): Unit

  def clear(): Unit

}
