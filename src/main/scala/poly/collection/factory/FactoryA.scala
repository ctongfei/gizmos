package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

trait FactoryA[+C[_]] {
  /** Creates an empty collection. */
  def empty[T]: C[T] = from(Traversable.empty)

  /** Creates a collection by adding the arguments into it. */
  def apply[T](xs: T*): C[T] = from(xs)

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T](xs: Traversable[T]): C[T]
}

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait BuilderFactoryA[+C[_]] extends FactoryA[C] {

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[T]: Builder[T, C[T]]

  /** Creates an empty collection. */
  override def empty[T]: C[T] = newBuilder[T].result

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAllInplace xs
    b.result
  }

}
