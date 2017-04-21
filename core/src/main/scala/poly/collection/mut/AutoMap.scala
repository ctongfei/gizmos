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
object AutoMap extends MapFactory[KeyMutableMap, Eq] {

  def newMapBuilder[K: Eq, V]: Builder[(K, V), KeyMutableMap[K, V]] = Eq[K] match {
    case kh: Hashing[K] => HashMap        .newMapBuilder[K, V](kh)
    case ko: Order[K]   => RedBlackTreeMap.newMapBuilder[K, V](ko)
    case ke             => ListMap        .newMapBuilder[K, V](ke)
  }

  object Dense extends MapFactory[KeyMutableMap, (Eq & ClassTag)#λ] {

    implicit def newMapBuilder[K, V](implicit K: (Eq & ClassTag)#λ[K]): Builder[(K, V), KeyMutableMap[K, V]] = K match {
      case Product2(std.IntStructure, ClassTag.Int) => DenseIntKeyedMap.newMapBuilder[K, V](evInt[K]).asInstanceOf[Builder[(K, V), KeyMutableMap[K, V]]] // this cast is safe because K =:= Int
      case _                                        => AutoMap.newMapBuilder(K._1)
    }
  }

}
