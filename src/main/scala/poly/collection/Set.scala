package poly.collection

import poly.algebra._
import poly.collection.builder._
import poly.collection.mut._

/**
 * Basic trait for sets whose elements can be iterated.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Set[T] extends Predicate[T] with Multiset[T] with KeyedLike[T, Set[T]] { self =>

  def equivOnKey: Equiv[T]

  /**
    * Returns an iterable sequence of all the elements in this set.
    * The elements returned should be distinct under the equivalence relation of this set.
    */
  def keys: Iterable[T]

  /** Tests if an element belongs to this set. */
  def contains(x: T): Boolean

  def apply(x: T) = contains(x)

  override def elements = keys

  final override def multiplicity(x: T) = if (contains(x)) 1 else 0

  final override def keySet = this

  def size = elements.size

  /**
    * Returns the union of two sets.
    * @example {{{ {1, 2, 3} | {2, 4} == {1, 2, 3, 4} }}}
    */
  def |(that: Set[T]): Set[T] = new AbstractSet[T] {
    def equivOnKey = self.equivOnKey
    def contains(x: T) = self.contains(x) || that.contains(x)
    def keys = self.keys ++ that.keys.filter(self.notContains)
  }

  /**
    * Returns the intersection of two sets.
    * @example {{{ {1, 2, 3} & {3, 1} == {1, 3} }}}
    */
  def &(that: Set[T]): Set[T] = new AbstractSet[T] {
    def equivOnKey = self.equivOnKey
    def contains(x: T) = self.contains(x) && that.contains(x)
    def keys = self.keys.filter(that.contains)
  }

  /**
    * Returns the difference of two sets.
    * @example {{{ {1, 2, 3} \ {2, 3} == {1} }}}
    */
  def \(that: Set[T]): Set[T] = new AbstractSet[T] {
    def equivOnKey: Equiv[T] = self.equivOnKey
    def contains(x: T): Boolean = self.contains(x) && that.notContains(x)
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


  def cartesianProduct[T1](that: Set[T1]): Set[(T, T1)] = new AbstractSet[(T, T1)] {
    def equivOnKey = Equiv.product(self.equivOnKey, that.equivOnKey)
    def keys = self.keys cartesianProduct that.keys
    override def size = self.size * that.size
    def contains(k: (T, T1)) = self.containsKey(k._1) && that.containsKey(k._2)
  }

  def createMapBy[V](f: T => V): Map[T, V] = new AbstractMap[T, V] {
    def apply(k: T) = f(k)
    def ?(k: T) = if (self contains k) Some(f(k)) else None
    def equivOnKey = self.equivOnKey
    def pairs = self.keys.map(k => k → f(k))
    override def size = self.size
    def containsKey(x: T) = self.contains(x)
  }

  def createMapByOptional[V](f: T => Option[V]): Map[T, V] = new AbstractMap[T, V] {
    def apply(k: T) = f(k).get
    def ?(k: T) = if (self contains k) f(k) else None
    def equivOnKey = self.equivOnKey
    def pairs = for (k ← self.keys; v ← f(k)) yield (k, v)
    def containsKey(x: T) = (self contains x) && f(x).isDefined
  }

  def createGraphBy[V, E](fv: T => V)(fe: (T, T) => Option[E]): Graph[T, V, E] = new AbstractGraph[T, V, E] {
    def apply(i: T) = fv(i)
    def containsNode(i: T) = self.contains(i)
    def containsEdge(i: T, j: T) = self.contains(i) && self.contains(j) && fe(i, j).isDefined
    def apply(i: T, j: T) = fe(i, j).get
    def outgoingKeysOf(i: T) = self.elements.filter(j => fe(i, j).isDefined)
    def keySet = self
  }

  override def filterKeys(f: T => Boolean): Set[T] = new AbstractSet[T] {
    def equivOnKey = self.equivOnKey
    def contains(x: T) = self.contains(x) && f(x)
    def keys = self.keys.filter(f)
  }

  def map[T1: Equiv](f: T => T1): Set[T1] = {
    self.elements.map(f).to(Set.autoBuilder[T1])
  }

  def zip(that: Set[T]): Set[T] = this & that

  def wrapKeysBy[T1](f: Bijection[T1, T]): Set[T1] = new AbstractSet[T1] {
    def equivOnKey = Equiv by f
    def keys = self.elements.map(f.invert)
    def contains(x: T1) = self.contains(f(x))
  }

  override def equals(that: Any) = that match {
    case that: Set[T] => Set.ContainmentOrder[T].eq(this, that)
    case _ => false
  }

  //Symbolic aliases
  def ⊂(that: Set[T]) = this properSubsetOf that
  def ⊃(that: Set[T]) = this properSupersetOf that
  def ⊆(that: Set[T]) = this subsetOf that
  def ⊇(that: Set[T]) = this supersetOf that
  def ∩(that: Set[T]) = this & that
  def ∪(that: Set[T]) = this | that
}

object Set {

  /**
   * Creates an empty set of a specific type.
   * @tparam T Type
   * @return An empty set
   */
  def empty[T: Equiv]: Set[T] = new Set[T] {
    def equivOnKey = implicitly[Equiv[T]]
    override def size = 0
    def keys = Iterable.empty
    def contains(x: T) = false
    override def |(that: Set[T]) = that
    override def \(that: Set[T]) = this
    override def &(that: Set[T]) = this
    override def subsetOf(that: Set[T]) = true
    override def properSubsetOf(that: Set[T]) = that.size != 0
  }

  /** Returns the lattice on sets. */
  implicit def Lattice[T]: Lattice[Set[T]] with BoundedLowerSemilattice[Set[T]] =
    new Lattice[Set[T]] with BoundedLowerSemilattice[Set[T]] {
      def bot = empty[T]
      def inf(x: Set[T], y: Set[T]) = x & y
      def sup(x: Set[T], y: Set[T]) = x | y
  }

  implicit def ContainmentOrder[T]: PartialOrder[Set[T]] = new PartialOrder[Set[T]] {
    override def eq(x: Set[T], y: Set[T]) = (x ⊆ y) && (x ⊇ y)
    def le(x: Set[T], y: Set[T]) = x ⊆ y
  }

  def autoBuilder[T](implicit e: Equiv[T]): Builder[T, Set[T]] = e match {
    case e: IntHashing[T] => HashSet.newBuilder[T] // HashSet.newBuilder(e)
    case e: WeakOrder[T] => ???
    case _ => ???
  }
}

abstract class AbstractSet[T] extends Set[T]
