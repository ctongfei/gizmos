package poly.collection.mut

import poly.algebra._
import poly.collection.builder._
import poly.collection.factory._

/**
 * @author Tongfei Chen
 */
object AutoMultimap extends BuilderFactoryAeBe[KeyMutableMultimap, Eq, Eq] {
  /** Returns a new builder of this collection type. */
  implicit def newBuilder[A: Eq, B: Eq] = ???
}
