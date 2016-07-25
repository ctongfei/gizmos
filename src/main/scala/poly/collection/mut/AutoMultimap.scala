package poly.collection.mut

import poly.algebra._
import poly.collection.builder._
import poly.collection.factory._

/**
 * @author Tongfei Chen
 */
/*
object AutoMultimap extends BuilderFactoryA_EvAB[KeyMutableMultimap, Eq, Eq] {

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[K, V](implicit K: Eq[K], V: Eq[V]): Builder[(K, V), KeyMutableMultimap[K, V]] = {
    def vsb = AutoSet.newBuilder(V).result
    val mm = (K, V) match {
      case (hk: Hashing[K]) =>
    }
  }
  
}
*/
