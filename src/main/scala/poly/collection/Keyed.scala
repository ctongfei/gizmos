package poly.collection

import poly.algebra._
import poly.algebra.specgroup._

/**
 * Marker trait for any structure that contains keys (unique identifiers).
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Keyed[@sp(i) K] {

  /** Returns the equivalence relation on keys. */
  def equivOnKey: Equiv[K]

  def containsKey(k: K): Boolean

  def keys: Iterable[K]

}
