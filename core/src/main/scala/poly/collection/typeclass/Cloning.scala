package poly.collection.typeclass

import scala.annotation._

/**
 * Represents a strategy for cloning an object of a specific type.
 * @author Tongfei Chen
 * @since 0.1.0
 */
@implicitNotFound("Cannot clone an object of type {T}.")
trait Cloning[X] {

  /** Clones this object. */
  def clone(x: X): X

}

object Cloning {

  def apply[T](implicit T: Cloning[T]): Cloning[T] = T

  def create[X](f: X => X) = new Cloning[X] {
    def clone(x: X): X = f(x)
  }

  implicit def anyVal[X <: AnyVal]: Cloning[X] = new Cloning[X] {
    def clone(x: X): X = x
  }

}
