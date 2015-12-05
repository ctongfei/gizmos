package poly.collection

import poly.algebra.specgroup._

/**
  * Represents a data structure that can be indexed by keys of a specific type.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.1.0
  */
trait KeyedStructure[@sp(i) K, +Coll <: KeyedStructure[K, Coll]] extends Keyed[K] {

  /** Tests if this structure contains an item with the specified key. */
  def containsKey(k: K): Boolean

  /** Returns an iterable sequence of all the keys in this structure. */
  def keys: Iterable[K]

  /**
    * Returns the restricted substructure obtained by choosing a smaller domain for the key.
    * @param f Key selector
    */
  def filterKeys(f: K => Boolean): Coll

}
