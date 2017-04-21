package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import scala.annotation._

/**
 * Represents a factory with all types parameters and/or implicit evidences filled (no ungrounded type parameters).
 * This factory
 *  - given a traversable collection whose elements are of type T,
 *  - builds an object of type R.
 * @author Tongfei Chen
 * @since 0.1.0
 */
@implicitNotFound("Cannot find a grounded factory to build a {R} from a traversable collection of {T}.")
trait Factory0[-T, +R] {

  /** Returns a new builder of this collection type. */
  def newBuilder: Builder[T, R]

  /** Creates an empty collection. */
  def empty: R = newBuilder.result()

  /** Creates a collection by adding the arguments into it. */
  def apply(xs: T*) = from(xs.asPoly)

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from(xs: Traversable[T]) = {
    val b = newBuilder
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAll xs
    b.result()
  }

}

object Factory0 {

  def ofBuilder[T, R](b: => Builder[T, R]): Factory0[T, R] = new Factory0[T, R] {
    def newBuilder = b
  }

}
