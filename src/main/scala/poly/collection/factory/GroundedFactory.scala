package poly.collection.factory

import poly.collection._
import poly.collection.conversion.FromScala._

/**
 * Represents a factory with all types parameters or/and implicit evidences filled.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait GroundedFactory[-T, +R] {

  def newBuilder: Builder[T, R]

  def empty: R = newBuilder.result()

  def apply(xs: T*) = from(xs)

  def from(xs: Traversable[T]) = {
    val b = newBuilder
    if (xs.sizeKnown) b.sizeHint(xs.size)
    b addAll xs
    b.result()
  }

}

object GroundedFactory {

  def ofBuilder[T, R](b: => Builder[T, R]): GroundedFactory[T, R] = new GroundedFactory[T, R] {
    def newBuilder = b
  }

}
