package poly.collection.mut

import poly.algebra._
import poly.collection.builder._
import poly.collection.factory._

/**
 * Constructs a mutable set given an implicit equivalence relation on the keys.
 * The type of the resulting set is determined from the following fallback relation:
 * <ul>
 *   <li> If the key is endowed with a hashing instance ([[poly.algebra.IntHashing]]),
 *     the result type is [[poly.collection.mut.HashSet]]. Under this condition, the lookup complexity is amortized O(1). </li>
 *   <li> Else, if the key is endowed with a weak order ([[poly.algebra.WeakOrder]]),
 *     the result type is [[poly.collection.mut.RedBlackTreeSet]]. Under this condition, the lookup complexity is O(''n'' log ''n''). </li>
 *   <li> Else, the result type is [[poly.collection.mut.ListSet]]. Under this condition, the lookup complexity is O(''n''). </li>
 * </ul>
 * @author Tongfei Chen
 * @since 0.1.0
 */
object AutoSet extends SetFactory[KeyMutableSet] {
  implicit def newBuilder[K](implicit K: Equiv[K]): Builder[K, KeyMutableSet[K]] = K match {
    case kh: IntHashing[K] => HashSet.newBuilder(kh)
    case ko: WeakOrder[K] => RedBlackTreeSet.newBuilder(ko)
    case ke => ListSet.newBuilder(ke)
  }
}
