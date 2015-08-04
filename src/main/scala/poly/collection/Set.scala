package poly.collection

import poly.collection.exception._
import poly.algebra._

/**
 * Basic trait for enumerable sets.
 * In Poly-collection, sets form a lattice.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Set[T] extends PredicateSet[T] with Multiset[T] {

  def multiplicity(x: T) = if (contains(x)) 1 else 0

  /** Tests if an element belongs to this set. */
  def contains(x: T): Boolean

  /** Returns the union of two sets. */
  def |(that: Set[T]): Set[T] = ???

  /** Returns the intersection of two sets. */
  def &(that: Set[T]): Set[T] = ???

  /** Returns the difference of two sets. */
  def \(that: Set[T]): Set[T] = ???

  /** Tests if this set is a subset of another set. */
  def <=(that: Set[T]): Boolean = this.forall(x => that.contains(x))

  /** Tests if this set is a strict subset of another set. */
  def <(that: Set[T]): Boolean = this.forall(x => that.contains(x)) && that.exists(x => !this.contains(x))

  /** Tests if this set is a strict superset of another set. */
  def >(that: Set[T]): Boolean = that < this

  /** Tests if this set is a superset of another set. */
  def >=(that: Set[T]): Boolean = that <= this

  override def equals(that: Any) = that match {
    case that: Set[T] => this.forall(x => that.contains(x)) && that.forall(x => this.contains(x))
    case that: Multiset[T] => that.equals(this)
    case _ => false
  }
}

object Set {

  /**
   * Creates an empty set of a specific type.
   * @tparam T Type
   * @return An empty set
   */
  def empty[T]: Set[T] = new Set[T] {
    override def size = 0
    def elements = Enumerable.empty
    def contains(x: T) = false
    override def |(that: Set[T]) = that
    override def \(that: Set[T]) = this
    override def &(that: Set[T]) = this
    override def <=(that: Set[T]) = true
    override def <(that: Set[T]) = that.size != 0
  }

  /** Returns the lattice on sets. */
  implicit def Lattice[T]: Lattice[Set[T]] with BoundedLowerSemilattice[Set[T]] =
    new Lattice[Set[T]] with BoundedLowerSemilattice[Set[T]] {
      def bottom = empty[T]
      def inf(x: Set[T], y: Set[T]) = x & y
      def sup(x: Set[T], y: Set[T]) = x | y
  }
}
