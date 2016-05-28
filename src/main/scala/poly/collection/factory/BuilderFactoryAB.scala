package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import poly.collection.builder._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait BuilderFactoryAB[+C[_, _]] extends FactoryAB[C] {

  implicit def newBuilder[A, B]: Builder[(A, B), C[A, B]]

  override def empty[A, B] = newBuilder[A, B].result

  def from[A, B](xs: Traversable[(A, B)]) = {
    val b = newBuilder[A, B]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAllInplace xs
    b.result
  }
}

trait FactoryAB[+C[_, _]] {

  def empty[A, B]: C[A, B] = from(Traversable.empty)

  def apply[A, B](xs: (A, B)*): C[A, B] = from(xs)

  def from[A, B](xs: Traversable[(A, B)]): C[A, B]

}
