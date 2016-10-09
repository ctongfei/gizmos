package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.AbstractSet
import poly.collection.builder._
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

  def removeInplace(x: K, w: R = weightRing.one) = {
    val u = data(x) - w
    if (u == weightRing.zero) data.removeInplace(x)
    data(x) = function.max(weightRing.zero, u)
  }

  def removeKeyInplace(x: K) = {
    data.removeInplace(x)
  }

  def weight(k: K) = data(k)

  def addInplace(x: K, w: R = weightRing.one) = {
    if (data containsKey x) data(x) += w
    else data addInplace(x, w)
  }
}

object PairWeightedSet extends BuilderFactoryA_EvAB[PairWeightedSet, Eq, OrderedRing] {

  /** Creates a factory of [[PairWeightedSet]]s given the type on counts. Normally [[R]] should be [[Int]]. */
  def of[R: OrderedRing]: BuilderFactoryA_EvA[({type λ[K] = PairWeightedSet[K, R]})#λ, Eq] = new BuilderFactoryA_EvA[({type λ[K] = PairWeightedSet[K, R]})#λ, Eq] {
    implicit def newBuilder[K: Eq] = PairWeightedSet.newBuilder[K, R]
  }

  implicit def newBuilder[K: Eq, R: OrderedRing]: Builder[K, PairWeightedSet[K, R]] = new Builder[K, PairWeightedSet[K, R]] {
    private[this] val ms = new PairWeightedSet[K, R](AutoMap[K, R]())
    def addInplace(x: K) = ms.addInplace(x)
    def result = ms
  }
}
