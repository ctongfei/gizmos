package poly.collection.impl.hashtable

import poly.collection.specgroup._

/**
 * Represents an entry in an open hash table.
 * This is similar to a `SeqNodeLike[K, E]`.
 * @author Tongfei Chen
 */
trait OpenHashEntryLike[@sp(Int) K, E <: OpenHashEntryLike[K, E]] { self: E =>

  def key: K
  var nextEntry: E = _

}
