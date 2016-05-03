package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import poly.collection.builder._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait Factory2[+C[_, _]] {

  implicit def newBuilder[A, B]: Builder[(A, B), C[A, B]]

  def empty[A, B] = newBuilder[A, B].result

  def apply[A, B](xs: (A, B)*): C[A, B] = from(xs)

  def from[A, B](xs: Traversable[(A, B)]) = {
    val b = newBuilder[A, B]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAllInplace xs
    b.result
  }
}
