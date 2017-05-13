package poly.collection.factory

import poly.collection._
import poly.collection.evidence._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait MapFactory[+M[_, _], Ev[_]] extends Factory2[Tuple2, M, Ev, NoneEv] {

  def newMapBuilder[K: Ev, V]: Builder[(K, V), M[K, V]]

  def newBuilder[K: Ev, V: NoneEv] = newMapBuilder[K, V]

  def empty[K: Ev] = newBuilder[K, Nothing].result()

  /**
   * Creates a map that restricts the given function on a specific iterable domain ([[Set]]).
   * @param f Function to be restricted
   * @param s Restricted domain
   * @note This is equivalent to `s createMap f` but eagerly evaluated.
   */
  def restrict[K: Ev, V](f: K => V, s: Set[K]) = {
    val b = newMapBuilder[K, V]
    for (x <- s) b << (x, f(x))
    b.result()
  }

}
