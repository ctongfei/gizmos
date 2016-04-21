package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.conversion.FromScala._
import poly.collection.builder._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait FactoryWithEquiv[+C[_]] {

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[T: Equiv]: Builder[T, C[T]]

  /** Creates an empty collection. */
  def empty[T: Equiv]: C[T] = newBuilder[T].result

  /** Creates a collection by adding the arguments into it. */
  def apply[T: Equiv](xs: T*): C[T] = {
    val b = newBuilder[T]
    b addAllInplace xs
    b.result
  }

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T: Equiv](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    b addAllInplace xs
    b.result
  }

}
