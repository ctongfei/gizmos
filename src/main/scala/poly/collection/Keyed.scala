package poly.collection

import poly.algebra._
import poly.algebra.specgroup._

/**
  * Marker trait for any data structure that contains keys (unique identifiers).
  * @author Tongfei Chen
  * @since 0.1.0
  * @define LAZY The resulting collection is '''lazily''' executed.
  * @define EAGER The resulting collection is '''eagerly''' executed.
 */
trait Keyed[@sp(i) K] {

  /** Returns the equivalence relation on keys. */
  //TODO: Case K=Int should be specialized to Equiv$mcI$sp to boost performance on maps/graphs
  //TODO: However, Scala compiler does not seem to do this
  def equivOnKey: Equiv[K]

}
