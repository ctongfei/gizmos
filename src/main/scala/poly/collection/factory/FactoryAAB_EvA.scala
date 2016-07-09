package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._
import poly.collection.builder._
import scala.language.higherKinds

/**
 * @author Tongfei Chen
 */
trait BuilderFactoryAAB_EvA[+G[_, _], Ev[_]] extends FactoryAAB_EvA[G, Ev] {

  implicit def newBuilder[A: Ev, B]: Builder[(A, A, B), G[A, B]]

  override def empty[A: Ev, B] = newBuilder[A, B].result

  def from[A: Ev, B](xs: Traversable[(A, A, B)]) = {
    val b = newBuilder[A, B]
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAllInplace xs
    b.result
  }
}

trait FactoryAAB_EvA[+G[_, _], Ev[_]] {

  def empty[A: Ev, B]: G[A, B] = from(Traversable.empty)

  def apply[A: Ev, B](xs: (A, A, B)*): G[A, B] = from(xs)

  def from[A: Ev, B](xs: Traversable[(A, A, B)]): G[A, B]
}
