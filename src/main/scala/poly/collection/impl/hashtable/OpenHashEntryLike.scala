package poly.collection.impl.hashtable

import poly.algebra.specgroup._

/**
 * Represents an entry in an open hash table.
 * This is similar to a SeqNode[K].
 * @author Tongfei Chen
 */
trait OpenHashEntryLike[@sp(i) K, E <: OpenHashEntryLike[K, E]] { self: E =>

  def key: K
  var nextEntry: E = _

}
