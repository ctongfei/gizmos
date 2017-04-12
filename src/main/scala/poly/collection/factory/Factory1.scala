package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

/**
 * Represents a factory that
 *  - given a traversable collection whose elements are of type E[T],
 *  - requiring an evidence of type Ev[T],
 *  - builds an object of type R[T].
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Factory1[-E[_], +R[_], Ev[_]] {

  /** Returns a new builder of this collection type. */
  def newBuilder[T: Ev]: Builder[E[T], R[T]]

  /** Grounds this factory by providing a type parameter and associated evidences. */
  implicit def ground[T: Ev] = Factory0 ofBuilder newBuilder[T]

  /** Creates an empty collection. */
  def empty[T: Ev]: R[T] = newBuilder[T].result

  /** Creates a collection by adding the arguments into it. */
  def apply[T: Ev](xs: E[T]*): R[T] = from(xs.asPoly)

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T: Ev](xs: Traversable[E[T]]): R[T] = {
    val b = newBuilder[T]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAll xs
    b.result
  }

}

