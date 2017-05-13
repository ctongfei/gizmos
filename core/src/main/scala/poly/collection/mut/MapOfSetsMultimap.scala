package poly.collection.mut

import poly.collection._

/**
 * Represents a multimap whose internal implementation is a map of keys to sets of values.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class MapOfSetsMultimap[K: Eq, V: Eq] private[collection] (private val data: KeyMutableMap[K, KeyMutableSet[V]]) extends KeyMutableMultimap[K, V] {

  def keySet = data.keySet

  def valueEq = Eq[V]

  /** Returns all values that are associated with the given key. */
  def apply(k: K): Set[V] = data(k)

  def add_!(k: K, v: V) =
    if (data notContainsKey k) data += (k, AutoSet(v))
    else data(k) += v

  def remove_!(k: K, v: V) =
    if (data containsKey k) data(k).remove_!(v)

  def removeAll_!(k: K) =
    data.remove_!(k)

  def clear_!() = data.clear_!()

}
