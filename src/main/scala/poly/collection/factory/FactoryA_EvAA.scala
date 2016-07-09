package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import poly.collection.builder._
import scala.language.higherKinds

trait BuilderFactoryA_EvAA[+C[_], Ev1[_], Ev2[_]] extends FactoryA_EvAA[C, Ev1, Ev2] {
  /** Returns a new builder of this collection type. */
  implicit def newBuilder[T : Ev1 : Ev2]: Builder[T, C[T]]

  /** Creates an empty collection. */
  override def empty[T : Ev1 : Ev2]: C[T] = newBuilder[T].result

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T : Ev1 : Ev2](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAllInplace xs
    b.result
  }

}

trait FactoryA_EvAA[+C[_], Ev1[_], Ev2[_]] {

  /** Creates an empty collection. */
  def empty[T : Ev1 : Ev2]: C[T] = from(Traversable.empty)

  /** Creates a collection by adding the arguments into it. */
  def apply[T : Ev1 : Ev2](xs: T*): C[T] = from(xs)

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T : Ev1 : Ev2](xs: Traversable[T]): C[T]

}