package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import poly.collection.builder._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait Factory2Ev[+C[_, _], Ev[_]] {

  implicit def newBuilder[A: Ev, B]: Builder[(A, B), C[A, B]]

  def empty[A: Ev, B] = newBuilder[A, B].result

  def apply[A: Ev, B](xs: (A, B)*): C[A, B] = from(xs)

  def from[A: Ev, B](xs: Traversable[(A, B)]) = {
    val b = newBuilder[A, B]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAllInplace xs
    b.result
  }
}
