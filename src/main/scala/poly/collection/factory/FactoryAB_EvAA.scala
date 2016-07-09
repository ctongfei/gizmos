package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import poly.collection.builder._
import scala.language.higherKinds

trait BuilderFactoryAB_EvAA[+C[_, _], Ev1[_], Ev2[_]] extends FactoryAB_EvAA[C, Ev1, Ev2] {
  /** Returns a new builder of this collection type. */
  implicit def newBuilder[A : Ev1 : Ev2, B]: Builder[(A, B), C[A, B]]

  /** Creates an empty collection. */
  override def empty[A : Ev1 : Ev2, B]: C[A, B] = newBuilder[A, B].result

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[A : Ev1 : Ev2, B](xs: Traversable[(A, B)]): C[A, B] = {
    val b = newBuilder[A, B]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAllInplace xs
    b.result
  }

}

trait FactoryAB_EvAA[+C[_, _], Ev1[_], Ev2[_]] {

  /** Creates an empty collection. */
  def empty[A : Ev1 : Ev2, B]: C[A, B] = from(Traversable.empty)

  /** Creates a collection by adding the arguments into it. */
  def apply[A : Ev1 : Ev2, B](xs: (A, B)*): C[A, B] = from(xs)

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[A : Ev1 : Ev2, B](xs: Traversable[(A, B)]): C[A, B]

}