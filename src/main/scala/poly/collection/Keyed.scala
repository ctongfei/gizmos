package poly.collection

import poly.algebra._
import poly.algebra.specgroup._

/**
  * Marker trait for any structure that contains keys (unique identifiers).
  * @author Tongfei Chen
  * @since 0.1.0
  * @define LAZY The resulting collection is '''lazily''' executed.
  * @define EAGER The resulting collection is '''eagerly''' executed.
 */
trait Keyed[@sp(i) K] {

  /** Returns the equivalence relation on keys. */
  implicit def equivOnKey: Equiv[K]

}
