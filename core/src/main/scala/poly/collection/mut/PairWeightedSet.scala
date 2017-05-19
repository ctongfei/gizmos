package poly.collection.mut

import spire.syntax.ring._
import poly.collection._
import poly.collection.factory._
import poly.collection.typeclass._

import scala.language.reflectiveCalls
import scala.language.higherKinds

/**
 * Represents a multiset whose underlying representation is a map of (key, weight) pairs.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class PairWeightedSet[K, R] private[poly](private val data: KeyMutableMap[K, R])(implicit val weightRing: Ring[R], val weightOrder: Order[R]) extends KeyMutableWeightedSet[K, R] {

  def keySet = data.keySet

  override def keyWeightPairs = data.pairs

  def remove_!(x: K, w: R = weightRing.one) = {
    val u = data(x) - w
    if (u == weightRing.zero) data.remove_!(x)
    data(x) = weightOrder.max(weightRing.zero, u)
  }

  def removeKey_!(x: K) = {
    data.remove_!(x)
  }

  def weight(k: K) = data(k)

  def add_!(x: K, w: R = weightRing.one) = {
    if (data containsKey x) data(x) = data(x) + w
    else data add_!(x, w)
  }
}

object PairWeightedSet extends Factory2[({type λ[K, R] = K})#λ, PairWeightedSet, Eq, (Order & Ring)#λ] {

  /** Creates a factory of [[PairWeightedSet]]s given the type on counts. Normally [[R]] should be [[Int]]. */
  def of[R: Order : Ring]: Factory1[Id, ({type λ[K] = PairWeightedSet[K, R]})#λ, Eq] = new Factory1[Id, ({type λ[K] = PairWeightedSet[K, R]})#λ, Eq] {
    implicit def newBuilder[K: Eq] = PairWeightedSet.newBuilder[K, R]
  }

  implicit def newBuilder[K: Eq, R](implicit R: Ev2[Order, Ring, R]): Builder[K, PairWeightedSet[K, R]] = new Builder[K, PairWeightedSet[K, R]] {
    private[this] val ms = new PairWeightedSet[K, R](AutoMap[K, R]())(R._2, R._1)
    def add(x: K) = ms.add_!(x)
    def result = ms
  }
}
