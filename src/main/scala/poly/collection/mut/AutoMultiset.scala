package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.factory._

/**
 * @author Tongfei Chen
 */
object AutoMultiset extends BuilderFactory1Ev12[KeyMutableWeightedSet, Eq, OrderedRing] {
  implicit def newBuilder[K, R](implicit K: Eq[K], R: OrderedRing[R]): Builder[K, KeyMutableWeightedSet[K, R]] = new Builder[K, KeyMutableWeightedSet[K, R]] {
    private[this] val ms = new PairWeightedSet[K, R](AutoMap[K, R]())
    def add(x: K) = ms += x
    def result = ms
  }
}
