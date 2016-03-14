package poly.collection

import poly.algebra._
import poly.algebra.specgroup._

/**
  * Represents a data structure that can be indexed by keys of a specific type.
  * @author Tongfei Chen
  * @since 0.1.0
  */
trait KeyedLike[@sp(i) K, +Coll <: KeyedLike[K, Coll]] extends Keyed[K] { self =>

  /** Tests if this structure contains an item with the specified key. */
  def containsKey(k: K): Boolean
  
  /** Tests if the specific key is absent in this structure. */
  final def notContainsKey(k: K) = !containsKey(k)

  /** Returns an iterable sequence of all the keys in this structure. */
  def keys: Iterable[K]

  def keySet: Set[K]

  /**
    * Returns the restricted substructure obtained by choosing a smaller domain for the key.
    * @param f Key selector
    */
  def filterKeys(f: K => Boolean): Coll


}
