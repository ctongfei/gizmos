package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.hkt.ops._

/**
 * Represents a pure, mathematical set (equivalent to a predicate).
 * A predicate set is contravariant on its type parameter and cannot be enumerated.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait PredicateSet[-T] extends (T => Boolean) { self =>

  def apply(x: T) = contains(x)

  def contains(x: T): Boolean

  def unary_! : PredicateSet[T] = new PredicateSet[T] {
    def contains(x: T) = !self.contains(x)
  }

  def &[U <: T](that: PredicateSet[U]): PredicateSet[U] = new PredicateSet[U] {
    def contains(x: U) = self.contains(x) && that.contains(x)
  }

  def |[U <: T](that: PredicateSet[U]): PredicateSet[U] = new PredicateSet[U] {
    def contains(x: U) = self.contains(x) || that.contains(x)
  }

  def contramap[U](f: U => T): PredicateSet[U] = new PredicateSet[U] {
    def contains(x: U) = self.contains(f(x))
  }
}

object PredicateSet {

  object empty extends PredicateSet[Any] {
    def contains(x: Any) = false
  }

  def universal[T]: PredicateSet[T] = new PredicateSet[T] {
    def contains(x: T) = true
  }
  /** Predicate sets form a contravariant functor. */
  implicit object ContravariantFunctor extends ContravariantFunctor[PredicateSet] {
    def contramap[X, Y](sx: PredicateSet[X])(f: Y => X): PredicateSet[Y] = sx contramap f
  }

  /** Predicate sets form a Boolean algebra. */
  implicit def BooleanAlgebra[T]: BooleanAlgebra[PredicateSet[T]] = new BooleanAlgebra[PredicateSet[T]] {
    def and(x: PredicateSet[T], y: PredicateSet[T]) = x & y
    def top = PredicateSet.universal[T]
    def not(x: PredicateSet[T]) = !x
    def or(x: PredicateSet[T], y: PredicateSet[T]) = x | y
    def bot = PredicateSet.empty
  }
  // Order will not be implemented: not computationally feasible on a Turing machine!

}
