package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.evidence._
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
object AutoSet extends SetFactory[KeyMutableSet, Eq] {
  implicit def newSetBuilder[K](implicit K: Eq[K]): Builder[K, KeyMutableSet[K]] = K match {
    case kh: Hashing[K] => HashSet.newBuilder(kh)
    case ko: Order[K]   => RedBlackTreeSet.newBuilder(ko)
    case ke             => ListSet.newBuilder(ke)
  }

  object Dense extends SetFactory[KeyMutableSet, (Eq & ClassTag)#λ] {
    implicit def newSetBuilder[K](implicit ev: Ev2[Eq, ClassTag, K]): Builder[K, KeyMutableSet[K]] = ev match {
      case Product2(std.IntStructure, ClassTag.Int) => BitSet.newSetBuilder(evInt[K]).asInstanceOf[Builder[K, KeyMutableSet[K]]] // the cast is safe: K =:= Int
      case _                                        => AutoSet.newSetBuilder(ev._1)
    }
  }
}
