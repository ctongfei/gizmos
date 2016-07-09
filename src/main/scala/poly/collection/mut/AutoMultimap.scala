package poly.collection.mut

import poly.algebra._
import poly.collection.builder._
import poly.collection.factory._

/**
 * @author Tongfei Chen
 */
object AutoMultimap extends BuilderFactoryA_EvAB[KeyMutableMultimap, Eq, Eq] {

  private def multimapBuilder[K, V](
    mapBuilder: Builder[(K, Set[V]), KeyMutableMap[K, Set[V]]],
    setBuilder: Builder[V, KeyMutableSet[V]]
  ): Builder[(K, V), KeyMutableMultimap[K, V] = new Builder[(K, V), KeyMutableMultimap[K, V]] {
    def addInplace(x: (K, V))
    def result = ???
}

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[K, V](implicit K: Eq[K], V: Eq[V]): Builder[(K, V), KeyMutableMultimap[K, V]] = {

  }
}
