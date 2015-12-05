package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.hkt.ops._

/**
 * Represents a pure, mathematical set (equivalent to a predicate).
 * A predicate set is contravariant on its type parameter and cannot be enumerated.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Predicate[-T] extends (T => Boolean) { self =>

  def apply(x: T) = contains(x)

  def contains(x: T): Boolean

  def unary_! : Predicate[T] = new Predicate[T] {
    def contains(x: T) = !self.contains(x)
  }

  def &[U <: T](that: Predicate[U]): Predicate[U] = new Predicate[U] {
    def contains(x: U) = self.contains(x) && that.contains(x)
  }

  def |[U <: T](that: Predicate[U]): Predicate[U] = new Predicate[U] {
    def contains(x: U) = self.contains(x) || that.contains(x)
  }

  def contramap[U](f: U => T): Predicate[U] = new Predicate[U] {
    def contains(x: U) = self.contains(f(x))
  }
}

object Predicate {

  object empty extends Predicate[Any] {
    def contains(x: Any) = false
  }

  def universal[T]: Predicate[T] = new Predicate[T] {
    def contains(x: T) = true
  }

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
  // Order will not be implemented: not computationally feasible on a Turing machine!

}
