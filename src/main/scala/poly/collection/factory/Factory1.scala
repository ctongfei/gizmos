package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Factory1[-E[_], +C[_], Ev[_]] {

  /** Returns a new builder of this collection type. */
  def newBuilder[T: Ev]: Builder[E[T], C[T]]

  /** Grounds this factory by providing a type parameter. */
  implicit def ground[T: Ev] = GroundedFactory ofBuilder newBuilder[T]

  /** Creates an empty collection. */
  def empty[T: Ev]: C[T] = newBuilder[T].result

  /** Creates a collection by adding the arguments into it. */
  def apply[T: Ev](xs: E[T]*): C[T] = from(xs)

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T: Ev](xs: Traversable[E[T]]): C[T] = {
    val b = newBuilder[T]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAll xs
    b.result
  }

}

