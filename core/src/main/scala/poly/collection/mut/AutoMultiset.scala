package poly.collection.mut

import poly.collection._
import poly.collection.factory._
import poly.collection.typeclass._

/**
 * @author Tongfei Chen
 */
object AutoMultiset extends Factory2[({type λ[K, R] = K})#λ, KeyMutableWeightedSet, Eq, (Order & Ring)#λ] {
???
  implicit def newBuilder[K, R](implicit K: Eq[K], R: Ev2[Order, Ring, R]): Builder[K, KeyMutableWeightedSet[K, R]] = new Builder[K, KeyMutableWeightedSet[K, R]] {
    private[this] val ms = new PairWeightedSet[K, R](AutoMap[K, R]())(R._2, R._1)
    def add(x: K) = ms += x
    def result = ms
  }
}
