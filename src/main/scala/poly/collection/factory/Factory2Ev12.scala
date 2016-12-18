package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

trait BuilderFactory2Ev12[+C[_, _], EvA[_], EvB[_]] extends Factory2Ev12[C, EvA, EvB] {
  /** Returns a new builder of this collection type. */
  implicit def newBuilder[A: EvA, B: EvB]: Builder[(A, B), C[A, B]]

  /** Creates an empty collection. */
  override def empty[A: EvA, B: EvB]: C[A, B] = newBuilder[A, B].result

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[A: EvA, B: EvB](xs: Traversable[(A, B)]): C[A, B] = {
    val b = newBuilder[A, B]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAll xs
    b.result
  }

}

trait Factory2Ev12[+C[_, _], EvA[_], EvB[_]] {

  /** Creates an empty collection. */
  def empty[A: EvA, B: EvB]: C[A, B] = from(Traversable.empty)

  /** Creates a collection by adding the arguments into it. */
  def apply[A: EvA, B: EvB](xs: (A, B)*): C[A, B] = from(xs)

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[A: EvA, B: EvB](xs: Traversable[(A, B)]): C[A, B]

}