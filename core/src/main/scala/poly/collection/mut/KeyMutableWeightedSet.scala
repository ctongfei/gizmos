package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait KeyMutableWeightedSet[K, R] extends WeightedSet[K, R] {

  def add_!(x: K, w: R = weightRing.one)

  def remove_!(x: K, w: R = weightRing.one)

  def removeKey_!(x: K)

  def multisetAdd_!(that: WeightedSet[K, R]) = for ((k, r) <- that.keyWeightPairs) add_!(k, r)

  def diff_!(that: WeightedSet[K, R]) = for ((k, r) <- that.keyWeightPairs) remove_!(k, r)

  final def +=(x: K, w: R = weightRing.one) = add_!(x, w)
  final def -=(x: K, w: R = weightRing.one) = remove_!(x, w)
}
