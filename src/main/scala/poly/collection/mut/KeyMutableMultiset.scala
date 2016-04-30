package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait KeyMutableMultiset[K, R] extends Multiset[K, R] {

  def addInplace(x: K, w: R = ringOnCount.one)

  def removeInplace(x: K, w: R = ringOnCount.one)

  def multisetAddInplace(that: Multiset[K, R]) = for ((k, r) ← that.pairs) addInplace(k, r)

  def diffInplace(that: Multiset[K, R]) = for ((k, r) ← that.pairs) removeInplace(k, r)

}
