package poly.collection

import poly.algebra._
import poly.algebra.syntax._

/**
  * Represents a multiset in which the same element can appear more than once.
  * @author Tongfei Chen
  * @since 0.1.0
  */
/**
trait Multiset[T, Z] extends Map[T, Z] { self =>

  implicit def groupOnValue: OrderedAdditiveCGroup[Z]

  def equivOnKey: Equiv[T]

  def keys: Iterable[T]

  def keySet: Set[T] = new AbstractSet[T] {
    def equivOnKey = self.equivOnKey
    def keys = self.keys
    def contains(x: T) = self.contains(x)
  }

  def contains(x: T): Boolean

  def notContains(x: T): Boolean = !contains(x)

  def containsKey(x: T) = contains(x)

  def multiplicity(x: T): Z

  //def elements: Iterable[T] = keys.flatMap(k => k.repeat(multiplicity(k)))

  def filterKeys(p: T => Boolean): Multiset[T, Z] = ???

  /**
    * Casts this multiset as a map that maps the unique elements to its multiplicity in this multiset. $LAZY
    * @example {{{ {1, 1, 1, 2}.asKeyFreqMap == {1 -> 3, 2 -> 1} }}}
    */
  def asKeyFreqMap: Map[T, Z] = new AbstractMap[T, Z] {
    def pairs = self.keys.map(k => k → self.multiplicity(k))
    def containsKey(x: T) = self.contains(x)
    def apply(k: T) = self.multiplicity(k)
    def ?(k: T) = if (self.contains(k)) Some(self.multiplicity(k)) else None
    def equivOnKey = self.equivOnKey
}

  // HELPER FUNCTIONS



  def |(that: Multiset[T, Z]): Multiset[T, Z] = new AbstractMultiset[T, Z] {
    def equivOnKey = self.equivOnKey
    implicit def groupOnValue = self.groupOnValue
    def multiplicity(x: T) = max(self.multiplicity(x), that.multiplicity(x))
    def keys = self.keys union that.keys
    def contains(x: T) = self.contains(x) || that.contains(x)
  }

  def &(that: Multiset[T, Z]): Multiset[T, Z] = new AbstractMultiset[T, Z] {
    def equivOnKey = self.equivOnKey
    implicit def groupOnValue = self.groupOnValue
    def multiplicity(x: T) = min(self.multiplicity(x), that.multiplicity(x))
    def keys = self.keys intersect that.keys
    def contains(x: T) = self.contains(x) && that.contains(x)
  }

  def +(that: Multiset[T, Z]): Multiset[T, Z] = new AbstractMultiset[T, Z] {
    def equivOnKey = self.equivOnKey
    implicit def groupOnValue = self.groupOnValue
    def multiplicity(x: T) = self.multiplicity(x) + that.multiplicity(x)
    def keys = self.keys union that.keys
    def contains(x: T) = self.contains(x) && that.contains(x)
  }

  def subsetOf(that: Multiset[T, Z]) =
    this.keys.forall(k => this.multiplicity(k) <= that.multiplicity(k))


  override def toString = ???

}

object Multiset {

  def empty[T: Equiv, Z: OrderedAdditiveCGroup]: Multiset[T, Z] = new AbstractMultiset[T, Z] {
    implicit def groupOnValue = implicitly[OrderedAdditiveCGroup[Z]]
    def equivOnKey = Equiv[T]
    def multiplicity(x: T) = zero[Z]
    def keys = Iterable.empty
    def contains(x: T) = false
  }

  /** Returns the lattice on sets. */
  implicit def Lattice[T: Equiv, Z: OrderedAdditiveCGroup]: Lattice[Multiset[T, Z]] with BoundedLowerSemilattice[Multiset[T, Z]] =
    new Lattice[Multiset[T, Z]] with BoundedLowerSemilattice[Multiset[T, Z]] {
      def bot = empty[T, Z]
      def inf(x: Multiset[T, Z], y: Multiset[T, Z]) = x & y
      def sup(x: Multiset[T, Z], y: Multiset[T, Z]) = x | y
    }

  implicit def ContainmentOrder[T: Equiv, Z: OrderedAdditiveCGroup]: PartialOrder[Multiset[T, Z]] = new PartialOrder[Multiset[T, Z]] {
    def le(x: Multiset[T, Z], y: Multiset[T, Z]) = x subsetOf y
  }
}

abstract class AbstractMultiset[T, Z] extends Multiset[T, Z]
*/