package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.builder._
import poly.collection.factory._
import scala.language.reflectiveCalls
import scala.language.higherKinds

/**
 * Represents a multiset whose underlying representation is a map of (key, weight) pairs.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class PairMultiset[K, R: OrderedRing] private(private val data: KeyMutableMap[K, R]) extends KeyMutableMultiset[K, R] {

  def eqOnKeys = data.eqOnKeys
  def ringOnWeight = OrderedRing[R]

  def removeInplace(x: K, w: R = ringOnWeight.one) = {
    val u = data(x) - w
    if (u == ringOnWeight.zero) data.removeInplace(x)
    data(x) = function.max(ringOnWeight.zero, u)
  }

  def removeAll(x: K) = {
    data.removeInplace(x)
  }

  def weight(k: K) = data(k)

  def keys = data.keys

  def contains(k: K) = data.containsKey(k)

  def addInplace(x: K, w: R = ringOnWeight.one) = {
    if (data containsKey x) data(x) += w
    else data addInplace(x, w)
  }
}

object PairMultiset extends BuilderFactoryEv2[PairMultiset, Eq, OrderedRing] {

  /** Creates a factory of PairMultisets given the type on counts. Normally [[R]] should be [[scala.Int]]. */
  def of[R: OrderedRing]: BuilderFactoryEv[({type λ[K] = PairMultiset[K, R]})#λ, Eq] = new BuilderFactoryEv[({type λ[K] = PairMultiset[K, R]})#λ, Eq] {
    implicit def newBuilder[K: Eq] = PairMultiset.newBuilder[K, R]
  }

  implicit def newBuilder[K: Eq, R: OrderedRing]: Builder[K, PairMultiset[K, R]] = new Builder[K, PairMultiset[K, R]] {
    private[this] val ms = new PairMultiset[K, R](AutoMap[K, R]())
    def addInplace(x: K) = ms.addInplace(x)
    def result = ms
    def sizeHint(n: Int) = {}
  }
}
