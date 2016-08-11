package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait KeyMutableMultiset[K, R] extends Multiset[K, R] {

  def addInplace(x: K, w: R = weightRing.one)

  def removeInplace(x: K, w: R = weightRing.one)

  def removeKeyInplace(x: K)

  def multisetAddInplace(that: Multiset[K, R]) = for ((k, r) <- that.keyWeightPairs) addInplace(k, r)

  def diffInplace(that: Multiset[K, R]) = for ((k, r) <- that.keyWeightPairs) removeInplace(k, r)

  final def +=(x: K, w: R = weightRing.one) = addInplace(x, w)
  final def -=(x: K, w: R = weightRing.one) = removeInplace(x, w)
}
