package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.builder._
import poly.collection.factory._

import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
class PairMultiset[K, R: OrderedRing] private(private val data: KeyMutableMap[K, R])
  extends KeyMutableMultiset[K, R] {

  def equivOnKeys = data.equivOnKeys
  def ringOnCount = OrderedRing[R]

  def removeInplace(x: K, w: R = ringOnCount.one) = {
    val u = data(x) - w
    if (u == zero[R]) data.removeInplace(x)
    data(x) = function.max(zero[R], u)
  }

  def removeAll(x: K) = {
    data.removeInplace(x)
  }

  def multiplicity(k: K) = data(k)

  def keys = data.keys

  def contains(k: K) = data.containsKey(k)

  def addInplace(x: K, w: R = ringOnCount.one) = data(x) += w
}

object PairMultiset extends FactoryEvEv[PairMultiset, Eq, OrderedRing] {

  implicit def newBuilder[K: Eq, R: OrderedRing]: Builder[K, PairMultiset[K, R]] = new Builder[K, PairMultiset[K, R]] {
    private[this] val ms = new PairMultiset[K, R](AutoMap[K, R]())
    def addInplace(x: K) = ms.addInplace(x)
    def result = ms
    def sizeHint(n: Int) = {}
  }
}
