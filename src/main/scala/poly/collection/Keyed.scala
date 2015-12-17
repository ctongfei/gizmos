package poly.collection

import poly.algebra._
import poly.algebra.specgroup._

/**
 * Marker trait for any structure that contains keys (unique identifiers).
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Keyed[@sp(i) K] {

  /** Returns the equivalence relation on keys. */
  implicit def equivOnKey: Equiv[K]

}
