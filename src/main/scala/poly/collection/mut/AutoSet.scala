package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.factory._

import scala.reflect._

/**
 * Constructs a mutable set given an implicit equivalence relation on the keys.
 * The type of the resulting set is determined from the following fallback relation:
 * <ul>
 *   <li> If the key is endowed with a hashing instance ([[poly.algebra.Hashing]]),
 *     the result type is [[poly.collection.mut.HashSet]]. Under this condition, the lookup complexity is amortized O(1). </li>
 *   <li> Else, if the key is endowed with a weak order ([[poly.algebra.Order]]),
 *     the result type is [[poly.collection.mut.RedBlackTreeSet]]. Under this condition, the lookup complexity is O(log ''n''). </li>
 *   <li> Else, the result type is [[poly.collection.mut.ListSet]]. Under this condition, the lookup complexity is O(''n''). </li>
 * </ul>
 * @author Tongfei Chen
 * @since 0.1.0
 */
object AutoSet extends BuilderFactory1Ev1[KeyMutableSet, Eq] {
  implicit def newBuilder[K](implicit K: Eq[K]): Builder[K, KeyMutableSet[K]] = K match {
    case kh: Hashing[K] => HashSet.newBuilder(kh)
    case ko: Order[K]   => RedBlackTreeSet.newBuilder(ko)
    case ke             => ListSet.newBuilder(ke)
  }

  object Dense extends BuilderFactory1Ev11[KeyMutableSet, Eq, ClassTag] {
    implicit def newBuilder[K](implicit K: Eq[K], ct: ClassTag[K]): Builder[K, KeyMutableSet[K]] = (K, ct) match {
      case (std.IntStructure, ClassTag.Int) => BitSet.newBuilder(evInt[K]).asInstanceOf[Builder[K, KeyMutableSet[K]]] // the cast is safe: K =:= Int
      case _                                => AutoSet.newBuilder(K)
    }
  }
}
