package poly.collection.impl.hashtable

import poly.algebra.specgroup._

/**
 * @author Tongfei Chen
 */
trait HashEntryLike[@sp(i) K, E <: HashEntryLike[K, E]] extends AnyRef { self: E =>

  val key: K
  var nextEntry: E = _

}
