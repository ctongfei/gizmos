package poly.collection

import poly.algebra.Lattice

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Multiset[T] extends Enumerable[T] { self =>

  /** Returns the multiplicity (number of occurrence) of an element in this multiset. */
  def multiplicity(x: T): Int

  /** Tests if an element belongs to this set. */
  def contains(x: T): Boolean

  /** Returns the union of two sets. */
  def |(that: Multiset[T]): Multiset[T]

  /** Returns the intersection of two sets. */
  def &(that: Multiset[T]): Multiset[T]

  /** Returns the difference of two sets. */
  def \(that: Multiset[T]): Multiset[T]

  /** Tests if this set is a subset of another set. */
  def <=(that: Multiset[T]): Boolean

  /** Tests if this set is a strict subset of another set. */
  def <(that: Multiset[T]): Boolean

  /** Tests if this set is a strict superset of another set. */
  def >(that: Multiset[T]): Boolean = that < this

  /** Tests if this set is a superset of another set. */
  def >=(that: Multiset[T]): Boolean = that <= this
}

object Multiset {

  implicit def Lattice[T]: Lattice[Multiset[T]] = new Lattice[Multiset[T]] {
    def inf(x: Multiset[T], y: Multiset[T]): Multiset[T] = x & y
    def sup(x: Multiset[T], y: Multiset[T]): Multiset[T] = x | y
  }

}
