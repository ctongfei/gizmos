package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Factory[+C[_]] {

  /** Returns a new builder of this collection type. */
  implicit def newBuilder[T]: Builder[T, C[T]]

  /** Creates an empty collection. */
  def empty[T]: C[T] = newBuilder[T].result

  /** Creates a collection by adding the arguments into it. */
  def apply[T](xs: T*): C[T] = {
    val b = newBuilder[T]
    b addAllInplace xs
    b.result
  }

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[T](xs: Traversable[T]): C[T] = {
    val b = newBuilder[T]
    b addAllInplace xs
    b.result
  }

  //implicit def factory: CollectionFactoryWithOrder[C] = this

}
