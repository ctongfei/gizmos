package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

trait BuilderFactory1Ev11[+C[_], Ev1[_], Ev2[_]] extends Factory1Ev11[C, Ev1, Ev2] {

  implicit def ground[T : Ev1 : Ev2] = GroundedFactory ofBuilder newBuilder[T]

  /** Returns a new builder of this collection type. */
  def newBuilder[T : Ev1 : Ev2]: Builder[T, C[T]]

  /** Creates an empty collection. */
  override def empty[T : Ev1 : Ev2]: C[T] = newBuilder[T].result

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T : Ev1 : Ev2](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAll xs
    b.result
  }

}

trait Factory1Ev11[+C[_], Ev1[_], Ev2[_]] {

  /** Creates an empty collection. */
  def empty[T : Ev1 : Ev2]: C[T] = from(Traversable.empty)

  /** Creates a collection by adding the arguments into it. */
  def apply[T : Ev1 : Ev2](xs: T*): C[T] = from(xs)

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T : Ev1 : Ev2](xs: Traversable[T]): C[T]

}