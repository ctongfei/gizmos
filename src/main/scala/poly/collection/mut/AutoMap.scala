package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.evidence._
import poly.collection.factory._

import scala.reflect._

/**
 * A mutable map factory that creates a map given an implicit equivalence relation on the keys.
 * The type of the resulting map is determined from the following fallback relation:
 * <ul>
 *   <li> If the key is endowed with a hashing instance ([[poly.algebra.Hashing]]),
 *     the result type is [[poly.collection.mut.HashMap]]. Under this condition, the lookup complexity is amortized O(1). </li>
 *   <li> Else, if the key is endowed with a weak order ([[poly.algebra.Order]]),
 *     the result type is [[poly.collection.mut.RedBlackTreeMap]]. Under this condition, the lookup complexity is O(log ''n''). </li>
 *   <li> Else, the result type is [[poly.collection.mut.ListMap]]. Under this condition, the lookup complexity is O(''n''). </li>
 * </ul>
 * @since 0.1.0
 * @author Tongfei Chen
 */
object AutoMap extends Factory2[Tuple2, KeyMutableMap, Eq, NoneEv] {

  def newBuilder[K: Eq, V: NoneEv]: Builder[(K, V), KeyMutableMap[K, V]] = Eq[K] match {
    case kh: Hashing[K] => HashMap        .newBuilder[K, V](kh, DummyImplicit.dummyImplicit)
    case ko: Order[K]   => RedBlackTreeMap.newBuilder[K, V](ko, DummyImplicit.dummyImplicit)
    case ke             => ListMap        .newBuilder[K, V](ke, DummyImplicit.dummyImplicit)
  }

  object Dense extends Factory2[Tuple2, KeyMutableMap, ({type λ[K] = Ev2[Eq, ClassTag, K]})#λ, NoneEv] {

    implicit def newBuilder[K, V](implicit K: Ev2[Eq, ClassTag, K], V: NoneEv[V]): Builder[(K, V), KeyMutableMap[K, V]] = K match {
      case Product2(std.IntStructure, ClassTag.Int) => DenseIntKeyedMap.newBuilder[K, V](evInt[K], DummyImplicit.dummyImplicit)
        .asInstanceOf[Builder[(K, V), KeyMutableMap[K, V]]] // this cast is safe because K =:= Int
      case _                                        => AutoMap.newBuilder(K._1, DummyImplicit.dummyImplicit)
    }
  }

}
