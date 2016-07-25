package poly.collection.mut

import poly.algebra._
import poly.collection.factory._
import poly.collection.builder._

/**
 * @author Tongfei Chen
 */
object AutoMultiset extends BuilderFactoryA_EvAB[KeyMutableMultiset, Eq, OrderedRing] {
  implicit def newBuilder[K, R](implicit K: Eq[K], R: OrderedRing[R]): Builder[K, KeyMutableMultiset[K, R]] = new Builder[K, KeyMutableMultiset[K, R]] {
    private[this] val ms = new PairMultiset[K, R](AutoMap[K, R]())
    def addInplace(x: K) = ms += x
    def result = ms
  }
}
