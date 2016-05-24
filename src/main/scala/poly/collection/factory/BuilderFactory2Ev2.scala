package poly.collection.factory

import poly.algebra._
import poly.collection._
import poly.collection.conversion.FromScala._
import poly.collection.builder._
import scala.language.higherKinds

/**
 * For factories that builds on a stream of A, but requiring evidences of type A and B.
 * @author Tongfei Chen
 */
trait BuilderFactory2Ev2[+C[_, _], EvA[_], EvB[_]] {
  /** Returns a new builder of this collection type. */
  implicit def newBuilder[A: EvA, B: EvB]: Builder[A, C[A, B]]

  /** Creates an empty collection. */
  def empty[A: EvA, B: EvB]: C[A, B] = newBuilder[A, B].result

  /** Creates a collection by adding the arguments into it. */
  def apply[A: EvA, B: EvB](xs: A*) = from(xs)

  /** Creates a collection by adding all the elements in the specific traversable sequence. */
  def from[A: EvA, B: EvB](xs: Traversable[A]): C[A, B] = {
    val b = newBuilder[A, B]
    b addAllInplace xs
    b.result
  }

}
