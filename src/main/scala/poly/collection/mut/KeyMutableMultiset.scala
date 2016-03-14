package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait KeyMutableMultiset[K, R] extends Multiset[K, R] {

  def add(x: K, w: R = ringOnCount.one)

  def remove(x: K, w: R = ringOnCount.one)

  def addInplace(that: Multiset[K, R]) = for ((k, r) ← that.pairs) add(k, r)

  def diffInplace(that: Multiset[K, R]) = for ((k, r) ← that.pairs) remove(k, r)

}
