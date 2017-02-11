package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

/**
 * Represents a factory of higher type [[C]] that when building a structure of type C[A],
 * requires evidence of type [[Ev]][A] being endowed on the actual type of the elements.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BuilderFactory1Ev1[+C[_], Ev[_]] extends Factory1Ev1[C, Ev] { self =>

  implicit def ground[T: Ev] = GroundedFactory ofBuilder newBuilder[T]

  /** Returns a new builder of this collection type. */
  def newBuilder[T: Ev]: Builder[T, C[T]]

  /** Creates an empty collection. */
  override def empty[T: Ev]: C[T] = newBuilder[T].result

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T: Ev](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAll xs
    b.result
  }

}

trait Factory1Ev1[+C[_], Ev[_]] {

  /** Creates an empty collection. */
  def empty[T: Ev]: C[T] = from(Traversable.empty)

  /** Creates a collection by adding the arguments into it. */
  def apply[T: Ev](xs: T*): C[T] = from(xs)

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T: Ev](xs: Traversable[T]): C[T]


}