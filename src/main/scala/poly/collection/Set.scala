package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._
import poly.collection.factory._
import poly.collection.impl._
import poly.collection.mut._

/**
 * Basic trait for sets whose elements can be iterated.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Set[@sp(Int) T] extends Predicate[T] with KeyedLike[T, Set[T]] { self =>

  implicit def eqOnKeys: Eq[T]

  /**
    * Returns an iterable sequence of all the elements in this set.
    * The elements returned should be distinct under the equivalence relation of this set.
    */
  def keys: Iterable[T]

  /** Tests if an element belongs to this set. */
  def contains(x: T): Boolean

  final def containsKey(x: T) = contains(x)

  final def notContains(x: T) = !contains(x)

  def apply(x: T) = contains(x)

  def elements = keys

  override def keySet = this

  def size = elements.size

  def isEmpty = size == 0

  /**
    * Returns the union of two sets.
    * @example {{{ {1, 2, 3} | {2, 4} == {1, 2, 3, 4} }}}
    */
  def union(that: Set[T]): Set[T] = new AbstractSet[T] {
    def eqOnKeys = self.eqOnKeys
    def contains(x: T) = self.contains(x) || that.contains(x)
    def keys = self.keys ++ that.keys.filter(self.notContains)
  }

  /**
    * Returns the intersection of two sets.
    * @example {{{ {1, 2, 3} & {1, 3, 5} == {1, 3} }}}
    */
  def intersect(that: Set[T]): Set[T] = new AbstractSet[T] {
    def eqOnKeys = self.eqOnKeys
    def contains(x: T) = self.contains(x) && that.contains(x)
    def keys = self.keys.filter(that.contains)
  }

  /**
    * Returns the set difference of two sets.
    * @example {{{ {1, 2, 3} \ {2, 3, 4} == {1} }}}
    */
  def setDiff(that: Set[T]): Set[T] = new AbstractSet[T] {
    def eqOnKeys = self.eqOnKeys
    def contains(x: T) = self.contains(x) && that.notContains(x)
    def keys = self.keys.filter(that.notContains)
  }

  /** Tests if this set is a subset of another set. */
  def subsetOf(that: Set[T]): Boolean = this forall that

  /** Tests if this set is a strict subset of another set. */
  def properSubsetOf(that: Set[T]): Boolean = (this forall that) && (that exists !this)

  /** Tests if this set is a strict superset of another set. */
  def supersetOf(that: Set[T]): Boolean = that subsetOf this

  /** Tests if this set is a superset of another set. */
  def properSupersetOf(that: Set[T]): Boolean = that properSubsetOf this

  /** Returns the cartesian product of two sets. */
  def product[U](that: Set[U]): Set[(T, U)] = new AbstractSet[(T, U)] {
    def eqOnKeys = Eq.product(self.eqOnKeys, that.eqOnKeys)
    def keys = self.keys product that.keys
    override def size = self.size * that.size
    def contains(k: (T, U)) = self.containsKey(k._1) && that.containsKey(k._2)
  }

  def cartesianProduct[U](that: Set[U]): Set[(T, U)] = this product that

  /** Using this set as the key set, construct a map by the given function. */
  def createMapBy[V](f: T => V): Map[T, V] = new AbstractMap[T, V] {
    def apply(k: T) = f(k)
    def ?(k: T) = if (self contains k) Some(f(k)) else None
    def eqOnKeys = self.eqOnKeys
    def pairs = self.keys.map(k => k → f(k))
    override def size = self.size
    def containsKey(x: T) = self.contains(x)
  }

  def createMapByOptional[V](f: T => Option[V]): Map[T, V] = new AbstractMap[T, V] {
    def apply(k: T) = f(k).get
    def ?(k: T) = if (self contains k) f(k) else None
    def eqOnKeys = self.eqOnKeys
    def pairs = for (k ← self.keys; v ← f(k)) yield (k, v)
    def containsKey(x: T) = (self contains x) && f(x).isDefined
  }

  def createGraphBy[V, E](fv: T => V)(fe: (T, T) => Option[E]): Graph[T, V, E] = new AbstractGraph[T, V, E] {
    def apply(i: T) = fv(i)
    def containsArc(i: T, j: T) = self.contains(i) && self.contains(j) && fe(i, j).isDefined
    def apply(i: T, j: T) = fe(i, j).get
    def outgoingKeysOf(i: T) = self.elements.filter(j => fe(i, j).isDefined)
    def keySet = self
  }

  override def filterKeys(f: T => Boolean): Set[T] = new AbstractSet[T] {
    def eqOnKeys = self.eqOnKeys
    def contains(x: T) = self.contains(x) && f(x)
    def keys = self.keys filter f
  }

  def map[U: Eq](f: T => U): Set[U] = elements map f to AutoSet

  def map[U](f: Bijection[T, U]): Set[U] = new AbstractSet[U] {
    def eqOnKeys = self.eqOnKeys contramap f.invert
    def keys = self.elements.map(f)
    def contains(x: U) = self contains f.invert(x)
  }

  def flatMap[U: Eq](f: T => Set[U]): Set[U] = {
    elements flatMap { x: T => f(x).elements } to AutoSet
  }

  def zip(that: Set[T]): Set[T] = this & that

  //TODO: !
//  def quotient(coarser: Eq[T]): Set[Set[T]] = new AbstractSet[Set[T]] {
//    private[this] val m = AutoMap[T, KeyMutableSet[T]]()(coarser)
//    def eqOnKeys = Set.Eq[T]
//    def keys = m.values
//    def contains(x: Set[T]) = {
//      if (x.isEmpty) false else Set.Eq[T].eq(x, m(x.elements.head))
//    }
//  }

  /**
   * Wraps each element of this element with a bijective function.
   * {{{
   *   Set[T]              S <=> T         Set[S]
   *    self  . contramap  (  f  )    ==   result
   * }}}
   * @example {{{
   *   {1, 2, 3} contramap {'A' <-> 1, 'B' <-> 2, 'C' <-> 3}
   *   == {'A', 'B', 'C'}
   * }}}
   */
  def contramap[S](f: Bijection[S, T]): Set[S] = new AbstractSet[S] {
    def eqOnKeys = self.eqOnKeys contramap f
    def keys = self.elements.map(f.invert)
    def contains(x: S) = self.contains(f(x))
  }

  def foreach[U](f: T => U) = elements foreach f

  def fold[U >: T](z: U)(f: (U, U) => U) = elements.fold(z)(f)

  def foldByMonoid[U >: T : Monoid] = elements.foldByMonoid[U]

  def reduce[U >: T](f: (U, U) => U) = elements reduce f

  def reduceBySemigroup[U >: T : Semigroup] = elements.reduceBySemigroup[U]

  def forall(f: T => Boolean) = elements forall f

  def exists(f: T => Boolean) = elements exists f

  def sum[U >: T : AdditiveCMonoid] = elements.sum[U]

  def max(implicit T: Order[T]) = elements.max

  def min(implicit T: Order[T]) = elements.min

  def minAndMax(implicit T: Order[T]) = elements.minAndMax

  def argmin[U: Order](f: T => U): T = elements.argmin(f)

  def minBy[U: Order](f: T => U) = argmin(f)

  def argmax[U: Order](f: T => U): T = elements.argmax(f)

  def maxBy[U: Order](f: T => U) = argmax(f)

  def argminWithValue[U](f: T => U)(implicit O: Order[U]) = elements.argminWithValue(f)(O)

  def argmaxWithValue[U](f: T => U)(implicit O: Order[U]) = elements.argmaxWithValue(f)(O)

  /**
   * Casts this set as a multiset in which each element appears exactly once.
   * @tparam R Type of the counts of elements in the multiset, can be `Int`, `Double`, etc.
   */
  def asMultiset[R: OrderedRing]: Multiset[T, R] = new AbstractMultiset[T, R] {
    def eqOnKeys = self.eqOnKeys
    def ringOnWeight = OrderedRing[R]
    def weight(k: T) = if (self.contains(k)) one[R] else zero[R]
    def keys = self.keys
    def contains(k: T) = self.contains(k)
  }

  //Symbolic aliases
  def &(that: Set[T]) = this intersect that
  def |(that: Set[T]) = this union that
  def &~(that: Set[T]) = this setDiff that
  def ⊂(that: Set[T]) = this properSubsetOf that
  def ⊃(that: Set[T]) = this properSupersetOf that
  def ⊆(that: Set[T]) = this subsetOf that
  def ⊇(that: Set[T]) = this supersetOf that
  def ∩(that: Set[T]) = this & that
  def ∪(that: Set[T]) = this | that
  def ∋(x: T) = this contains x
  def ∌(x: T) = this notContains x

  override def toString = "{" + elements.buildString(", ") + "}"

  override def equals(that: Any) = that match {
    case that: Set[T] => (this subsetOf that) && (this supersetOf that)
    case _ => false
  }

  override def hashCode = MurmurHash3.symmetricHash(self.elements)(Hashing.default[T])
}

object Set extends FactoryEv[Set, Eq] {

  // CONSTRUCTORS

  def from[T: Eq](xs: Traversable[T]) = AutoSet from xs

  /** Creates an empty set of a specific type. */
  override def empty[T: Eq]: Set[T] = new Set[T] {
    def eqOnKeys = poly.algebra.Eq[T]
    override def size = 0
    def keys = Iterable.empty
    def contains(x: T) = false
    override def union(that: Set[T]) = that
    override def setDiff(that: Set[T]) = this
    override def intersect(that: Set[T]) = this
    override def subsetOf(that: Set[T]) = true
    override def properSubsetOf(that: Set[T]) = that.size != 0
  }

  // TYPECLASSES INSTANCES

  /** Returns the lattice on sets. */
  implicit def Lattice[T: Eq]: Lattice[Set[T]] with BoundedLowerSemilattice[Set[T]] =
    new Lattice[Set[T]] with BoundedLowerSemilattice[Set[T]] {
      def bot = empty[T]
      def inf(x: Set[T], y: Set[T]) = x ∩ y
      def sup(x: Set[T], y: Set[T]) = x ∪ y
  }

  implicit def Hashing[T: Hashing]: Hashing[Set[T]] = new Hashing[Set[T]] {
    def eq(x: Set[T], y: Set[T]) = (x ⊆ y) && (x ⊇ y)
    def hash(x: Set[T]) = MurmurHash3.symmetricHash(x.elements)
  }

  def ContainmentOrder[T]: PartialOrder[Set[T]] = new PartialOrder[Set[T]] {
    override def eq(x: Set[T], y: Set[T]) = (x ⊆ y) && (x ⊇ y)
    def le(x: Set[T], y: Set[T]) = x ⊆ y
  }

}

abstract class AbstractSet[@sp(Int) T] extends Set[T]
