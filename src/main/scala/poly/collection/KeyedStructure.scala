package poly.collection

import poly.algebra.specgroup._

/**
  * Represents a data structure that can be indexed by keys of a specific type.
  * @author Tongfei Chen (ctongfei@gmail.com).
  * @since 0.1.0
  */
trait KeyedStructure[@sp(i) K, +Repr <: KeyedStructure[K, Repr]] extends Keyed[K] {

  def filterKeys(f: K => Boolean): Repr

}
