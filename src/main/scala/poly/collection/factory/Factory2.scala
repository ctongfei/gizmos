package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

/**
 * Represents a factory that
 *  - given a traversable collection whose elements are of type E[A, B],
 *  - requiring an evidence of type Ev[A] and Ev[B],
 *  - builds an object of type R[A, B].
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Factory2[-E[_, _], +R[_, _], Ev1[_], Ev2[_]] {

  /** Returns a new builder of this collection type. */
  def newBuilder[A : Ev1, B: Ev2]: Builder[E[A, B], R[A, B]]

  /** Grounds this factory by providing a type parameter and associated evidences. */
  implicit def ground[A : Ev1, B : Ev2] = Factory0 ofBuilder newBuilder[A, B]

  /** Creates an empty collection. */
  def empty[A : Ev1, B: Ev2] = newBuilder[A, B].result()

  /** Creates a collection by adding the arguments into it. */
  def apply[A : Ev1, B : Ev2](xs: E[A, B]*): R[A, B] = from(xs)

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[A : Ev1, B: Ev2](xs: Traversable[E[A, B]]) = {
    val b = newBuilder[A, B]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAll xs
    b.result
  }
}

