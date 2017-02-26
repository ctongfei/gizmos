package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._

import scala.language.higherKinds

trait Factory2[-E[_, _], +C[_, _], Ev1[_], Ev2[_]] {

  implicit def ground[A : Ev1, B : Ev2] = GroundedFactory ofBuilder newBuilder[A, B]

  def newBuilder[A : Ev1, B: Ev2]: Builder[E[A, B], C[A, B]]

  def empty[A : Ev1, B: Ev2] = newBuilder[A, B].result()

  def apply[A : Ev1, B : Ev2](xs: E[A, B]*): C[A, B] = from(xs)

  def from[A : Ev1, B: Ev2](xs: Traversable[E[A, B]]) = {
    val b = newBuilder[A, B]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAll xs
    b.result
  }
}

