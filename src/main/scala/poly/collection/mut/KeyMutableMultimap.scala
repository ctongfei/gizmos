package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait KeyMutableMultimap[K, V] extends Multimap[K, V] {

  def add_!(k: K, v: V): Unit

  def add_!(kv: (K, V)): Unit = add_!(kv._1, kv._2)

  def remove_!(k: K, v: V): Unit

  def removeAll_!(k: K): Unit

  def clear_!(): Unit

  final def +=(k: K, v: V) = add_!(k, v)
  final def +=(kv: (K, V)) = add_!(kv)
  final def -=(k: K, v: V) = remove_!(k, v)
  final def -=(k: K) = removeAll_!(k)

}
