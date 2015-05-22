package poly.collection

import poly.collection.exception._
import poly.algebra._

/**
 * Basic trait for sets.
 * In Poly-collection, sets form a Boolean algebra.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Set[+T] extends Multiset[T] with Enumerable[T] {

  def multiplicity[U >: T](x: U) = if (contains(x)) 1 else 0

  /** Tests if an element belongs to this set. */
  def contains[U >: T](x: U): Boolean

  /** Returns the complement set of this set. */
  def complement: Set[T] = new ComplementSet[T](this)

  /** Symbolic alias for `complement`. */
  def unary_! : Set[T] = complement

  /** Returns the union of two sets. */
  def |[U >: T](that: Set[U]): Set[U]

  /** Returns the intersection of two sets. */
  def &[U >: T](that: Set[U]): Set[U]

  /** Returns the difference of two sets. */
  def \[U >: T](that: Set[U]): Set[U]

  /** Tests if this set is a subset of another set. */
  def <=[U >: T](that: Set[U]): Boolean

  /** Tests if this set is a strict subset of another set. */
  def <[U >: T](that: Set[U]): Boolean

  /** Tests if this set is a strict superset of another set. */
  def >[U >: T](that: Set[U]): Boolean = that < this

  /** Tests if this set is a superset of another set. */
  def >=[U >: T](that: Set[U]): Boolean = that <= this
}

object Set {
  def empty[T]: Set[T] = new Set[T] {
    def enumerator = Enumerator.empty[T]
    def contains[U >: T](x: U) = false
    def |[U >: T](that: Set[U]) = that
    def &[U >: T](that: Set[U]) = this
    def \[U >: T](that: Set[U]) = this
    def <=[U >: T](that: Set[U]) = true
    def <[U >: T](that: Set[U]) = that.size != 0
  }

  def universal[T]: Set[T] = !empty[T]

  implicit def BooleanAlgebra[T]: BooleanAlgebra[Set[T]] = new BooleanAlgebra[Set[T]] {
    def and(x: Set[T], y: Set[T]) = x & y
    def or(x: Set[T], y: Set[T]) = x | y
    def not(x: Set[T]) = !x
    def zero = Set.empty[T]
    def one = Set.universal[T]
    override def eq(x: Set[T], y: Set[T]) = x == y //TODO: by set value!
  }
}

class ComplementSet[T](override val complement: Set[T]) extends Set[T] {
  def contains[U >: T](x: U): Boolean = !complement.contains(x)
  def enumerator = throw new SetInfiniteException
  def |[U >: T](that: Set[U]): Set[U] = !(this \ that)
  def &[U >: T](that: Set[U]): Set[U] = that \ !this
  def \[U >: T](that: Set[U]): Set[U] = !(this | that)
  def <=[U >: T](that: Set[U]) = (!that) <= (!this)
  def <[U >: T](that: Set[U]) = (!that) < (!this)
}
