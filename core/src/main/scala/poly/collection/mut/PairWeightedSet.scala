package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.{AbstractSet, Builder}
import poly.collection.factory._

import scala.language.reflectiveCalls
import scala.language.higherKinds

/**
 * Represents a multiset whose underlying representation is a map of (key, weight) pairs.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class PairWeightedSet[K, R: OrderedRing] private[poly](private val data: KeyMutableMap[K, R]) extends KeyMutableWeightedSet[K, R] {

  def keySet = data.keySet

  def weightRing = OrderedRing[R]

  override def keyWeightPairs = data.pairs

  def remove_!(x: K, w: R = weightRing.one) = {
    val u = data(x) - w
    if (u == weightRing.zero) data.remove_!(x)
    data(x) = function.max(weightRing.zero, u)
  }

  def removeKey_!(x: K) = {
    data.remove_!(x)
  }

  def weight(k: K) = data(k)

  def add_!(x: K, w: R = weightRing.one) = {
    if (data containsKey x) data(x) += w
    else data add_!(x, w)
  }
}

object PairWeightedSet extends Factory2[({type λ[K, R] = K})#λ, PairWeightedSet, Eq, OrderedRing] {

  /** Creates a factory of [[PairWeightedSet]]s given the type on counts. Normally [[R]] should be [[Int]]. */
  def of[R: OrderedRing]: Factory1[Id, ({type λ[K] = PairWeightedSet[K, R]})#λ, Eq] = new Factory1[Id, ({type λ[K] = PairWeightedSet[K, R]})#λ, Eq] {
    implicit def newBuilder[K: Eq] = PairWeightedSet.newBuilder[K, R]
  }

  implicit def newBuilder[K: Eq, R: OrderedRing]: Builder[K, PairWeightedSet[K, R]] = new Builder[K, PairWeightedSet[K, R]] {
    private[this] val ms = new PairWeightedSet[K, R](AutoMap[K, R]())
    def add(x: K) = ms.add_!(x)
    def result = ms
  }
}
