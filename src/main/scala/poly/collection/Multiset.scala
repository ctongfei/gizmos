package poly.collection

import poly.algebra._
import poly.collection.ops._

/**
  * Represents a multiset in which the same element can appear more than once.
  * @author Tongfei Chen
  * @since 0.1.0
  */
trait Multiset[T] extends KeyedLike[T, Multiset[T]] { self =>

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

  def multiplicity(x: T): Int

  def elements: Iterable[T] = keys.flatMap(k => k.repeat(multiplicity(k)))

  def filterKeys(p: T => Boolean): Multiset[T] = ???

  /**
    * Casts this multiset as a map that maps the unique elements to its multiplicity in this multiset. $LAZY
    * @example {{{ {1, 1, 1, 2}.asKeyFreqMap == {1 -> 3, 2 -> 1} }}}
    */
  def asKeyFreqMap: Map[T, Int] = new AbstractMap[T, Int] {
    def pairs = self.keys.map(k => k → self.multiplicity(k))
    def containsKey(x: T) = self.contains(x)
    def apply(k: T) = self.multiplicity(k)
    def ?(k: T) = if (self.contains(k)) Some(self.multiplicity(k)) else None
    def equivOnKey = self.equivOnKey
}

  // HELPER FUNCTIONS

  def foreach[U](f: T => U) = elements foreach f

  def fold[U >: T](z: U)(f: (U, U) => U) = elements.fold(z)(f)

  def foldByMonoid[U >: T : Monoid] = elements.foldByMonoid[U]

  def reduce[U >: T](f: (U, U) => U) = elements reduce f

  def reduceBySemigroup[U >: T : Semigroup] = elements.reduceBySemigroup[U]

  def forall(f: T => Boolean) = keys forall f

  def exists(f: T => Boolean) = keys exists f

  def sum[U >: T : AdditiveCMonoid] = keys.sum[U]

  def max(implicit T: WeakOrder[T]) = keys.max

  def min(implicit T: WeakOrder[T]) = keys.min

  def minAndMax(implicit T: WeakOrder[T]) = keys.minAndMax

  def |(that: Multiset[T]): Multiset[T] = new AbstractMultiset[T] {
    def equivOnKey = self.equivOnKey
    def multiplicity(x: T) = math.max(self.multiplicity(x), that.multiplicity(x))
    def keys = self.keys union that.keys
    def contains(x: T) = self.contains(x) || that.contains(x)
  }

  def &(that: Multiset[T]): Multiset[T] = new AbstractMultiset[T] {
    def equivOnKey = self.equivOnKey
    def multiplicity(x: T) = math.min(self.multiplicity(x), that.multiplicity(x))
    def keys = self.keys intersect that.keys
    def contains(x: T) = self.contains(x) && that.contains(x)
  }

  override def toString = "{" + elements.buildString(",") + "}"

}

object Multiset {
  /** Returns the lattice on sets. */
  implicit def Lattice[T]: Lattice[Multiset[T]] with BoundedLowerSemilattice[Multiset[T]] =
    new Lattice[Multiset[T]] with BoundedLowerSemilattice[Multiset[T]] {
      def bot = Set.empty[T]
      def inf(x: Multiset[T], y: Multiset[T]) = x & y
      def sup(x: Multiset[T], y: Multiset[T]) = x | y
    }

  implicit def ContainmentOrder[T]: PartialOrder[Multiset[T]] = new PartialOrder[Multiset[T]] {
    def le(x: Multiset[T], y: Multiset[T]) = ???
  }
}

abstract class AbstractMultiset[T] extends Multiset[T]
