package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait KeyMutableWeightedSet[K, R] extends WeightedSet[K, R] {

  def addInplace(x: K, w: R = weightRing.one)

  def removeInplace(x: K, w: R = weightRing.one)

  def removeKeyInplace(x: K)

  def multisetAddInplace(that: WeightedSet[K, R]) = for ((k, r) <- that.keyWeightPairs) addInplace(k, r)

  def diffInplace(that: WeightedSet[K, R]) = for ((k, r) <- that.keyWeightPairs) removeInplace(k, r)

  final def +=(x: K, w: R = weightRing.one) = addInplace(x, w)
  final def -=(x: K, w: R = weightRing.one) = removeInplace(x, w)
}
