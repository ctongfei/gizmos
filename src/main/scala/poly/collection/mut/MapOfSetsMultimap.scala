package poly.collection.mut

import poly.algebra._
import poly.collection._

/**
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
class MapOfSetsMultimap[K: Eq, V: Eq] private[collection] (private val data: KeyMutableMap[K, KeyMutableSet[V]]) extends KeyMutableMultimap[K, V] {

  def keySet = data.keySet

  def valueEq = Eq[V]

  /** Returns all values that are associated with the given key. */
  def apply(k: K): Set[V] = data(k)

  def addInplace(k: K, v: V) =
    if (data notContainsKey k) data += (k, AutoSet(v))
    else data(k) += v

  def removeInplace(k: K, v: V) =
    if (data containsKey k) data(k).removeInplace(v)

  def removeAllInplace(k: K) =
    data.removeInplace(k)

  def clear() = data.clear()

}
