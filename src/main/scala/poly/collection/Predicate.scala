package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import scala.language.implicitConversions

/**
 * Represents a pure, mathematical set (equivalent to a predicate).
 * A predicate set is contravariant on its type parameter and cannot be iterated.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Predicate[-T] extends (T => Boolean) { self =>

  def unary_! : Predicate[T] = new Predicate[T] {
    def apply(x: T) = !self(x)
  }

  def &[U <: T](that: Predicate[U]): Predicate[U] = new Predicate[U] {
    def apply(x: U) = self(x) && that(x)
  }

  def |[U <: T](that: Predicate[U]): Predicate[U] = new Predicate[U] {
    def apply(x: U) = self(x) || that(x)
  }

  def contramap[U](f: U => T): Predicate[U] = new Predicate[U] {
    def apply(x: U) = self(f(x))
  }
}

object Predicate {

  // CONSTRUCTORS

  object empty extends Predicate[Any] {
    def apply(x: Any) = false
  }

  def universal[T]: Predicate[T] = new Predicate[T] {
    def apply(x: T) = true
  }

  // IMPLICIT CONVERSIONS

  implicit def fromFuncToBool[T](f: T => Boolean): Predicate[T] = new Predicate[T] {
    def apply(x: T) = f(x)
  }

  // TYPECLASS INSTANCES

  /** Predicate sets form a contravariant functor. */
  implicit object ContravariantFunctor extends ContravariantFunctor[Predicate] {
    def contramap[X, Y](sx: Predicate[X])(f: Y => X): Predicate[Y] = sx contramap f
  }

  /** Predicate sets form a Boolean algebra. */
  implicit def BooleanAlgebra[T]: BooleanAlgebra[Predicate[T]] = new BooleanAlgebra[Predicate[T]] {
    def and(x: Predicate[T], y: Predicate[T]) = x & y
    def top = Predicate.universal[T]
    def not(x: Predicate[T]) = !x
    def or(x: Predicate[T], y: Predicate[T]) = x | y
    def bot = Predicate.empty
  }
  // Order or Equiv will not be implemented: not computationally feasible on a Turing machine!

}
