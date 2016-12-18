package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import scala.language.higherKinds
/**
 * @author Tongfei Chen
 */
trait BuilderFactoryInt2[+C[_]] extends FactoryInt2[C] {

  implicit def newBuilder[A]: Builder[(Int, A), C[A]]

  override def empty[A] = newBuilder[A].result

  def from[A](xs: Traversable[(Int, A)]) = {
    val b = newBuilder[A]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAll xs
    b.result
  }
}

trait FactoryInt2[+C[_]] {

  def empty[A]: C[A] = from(Traversable.empty)

  def apply[A](xs: (Int, A)*): C[A] = from(xs)

  def from[A](xs: Traversable[(Int, A)]): C[A]

}

