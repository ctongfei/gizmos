package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.builder._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
class PairMultiset[K: IntHashing, R: OrderedRing] private(private val data: HashMap[K, R])
  extends KeyMutableMultiset[K, R] {

  def equivOnKey = Equiv[K]
  def ringOnCount = OrderedRing[R]

  def remove(x: K, w: R = ringOnCount.one) = {
    val u = data(x) - w
    data(x) = max(ringOnCount.zero, u)
  }

  def multiplicity(k: K) = data(k)

  def keys = data.keys

  def containsKey(k: K) = data.containsKey(k)

  def add(x: K, w: R = ringOnCount.one) = data(x) += w
}

object PairMultiset {

  implicit def newBuilder[K: IntHashing, R: OrderedRing] = ???

}
