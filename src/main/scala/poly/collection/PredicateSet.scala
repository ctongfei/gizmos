package poly.collection

import poly.algebra._
import poly.algebra.hkt._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
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

 // implicit object ContravariantFunctor extends ContravariantFunctor[PredicateSet] {

 // }

  implicit def BooleanAlgebra[T]: BooleanAlgebra[PredicateSet[T]] = new BooleanAlgebra[PredicateSet[T]] {
    def and(x: PredicateSet[T], y: PredicateSet[T]) = x & y
    def one = PredicateSet.universal[T]
    def not(x: PredicateSet[T]) = !x
    def or(x: PredicateSet[T], y: PredicateSet[T]) = x | y
    def zero = PredicateSet.empty
  }

}