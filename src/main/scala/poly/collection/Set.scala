package poly.collection

import poly.algebra._
import poly.collection.mut._

/**
 * Basic trait for sets whose elements can be enumerated.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Set[T] extends PredicateSet[T] with KeyedStructure[T, Set[T]] { self =>

  // ListSet: Equiv[T]
  // HashSet: IntHashing[T]
  // TreeSet: WeakOrder[T]
  def equivOnKey: Equiv[T]



  def elements: Iterable[T]

  def distinct = self

  /** Tests if an element belongs to this set. */
  def contains(x: T): Boolean

  /** Tests if an element does not belong to this set. */
  def notContains(x: T) = !contains(x)

  def containsKey(x: T) = contains(x)

  final override def keys = elements

  def size = elements.size

  def foreach[U](f: T => U) = elements foreach f

  def fold[U >: T](z: U)(f: (U, U) => U) = elements.fold(z)(f)

  def reduce[U >: T](f: (U, U) => U) = elements.reduce(f)

  def forall(f: T => Boolean) = elements.forall(f)

  def exists(f: T => Boolean) = elements.exists(f)

  /**
    * Returns the union of two sets.
    * @example {{{ {1, 2, 3} | {2, 4} == {1, 2, 3, 4} }}}
    */
  def |(that: Set[T]): Set[T] = new AbstractSet[T] {
    def equivOnKey = self.equivOnKey
    def contains(x: T) = self.contains(x) && that.contains(x)
    def elements = self.elements ++ that.elements.filter(self.notContains)
  }

  /**
    * Returns the intersection of two sets.
    * @example {{{ {1, 2, 3} & {3, 1} == {1, 3} }}}
    */
  def &(that: Set[T]): Set[T] = new AbstractSet[T] {
    def equivOnKey = self.equivOnKey
    def contains(x: T) = self.contains(x) && that.contains(x)
    def elements = self.elements.filter(that.contains)
  }

  /**
    * Returns the difference of two sets.
    * @example {{{ {1, 2, 3} \ {2, 3} == {1} }}}
    */
  def \(that: Set[T]): Set[T] = new AbstractSet[T] {
    def equivOnKey: Equiv[T] = self.equivOnKey
    def contains(x: T): Boolean = self.contains(x) && that.notContains(x)
    def elements = self.elements.filter(that.notContains)
  }

  /** Tests if this set is a subset of another set. */
  def <=(that: Set[T]): Boolean = this forall that

  /** Tests if this set is a strict subset of another set. */
  def <(that: Set[T]): Boolean = (this forall that) && (that exists !this)

  /** Tests if this set is a strict superset of another set. */
  def >(that: Set[T]): Boolean = that < this

  /** Tests if this set is a superset of another set. */
  def >=(that: Set[T]): Boolean = that <= this

  def createMapBy[V](f: T => V): Map[T, V] = new AbstractMap[T, V] {
    def apply(k: T) = f(k)
    def ?(k: T) = if (self.contains(k)) Some(f(k)) else None
    def equivOnKey = self.equivOnKey
    def pairs = self.keys.map(k => k → f(k))
    override def size = self.size
    def containsKey(x: T) = self.contains(x)
  }


  override def filterKeys(f: T => Boolean): Set[T] = new AbstractSet[T] {
    def equivOnKey = self.equivOnKey
    def contains(x: T) = self.contains(x) && f(x)
    def elements = self.elements.filter(f)
  }

  def mapKeys[U](f: T => U)(implicit U: Equiv[U]): Set[U] = {
    self.elements.map(f).to(Set.autoBuilder(U))
  }

  override def equals(that: Any) = that match {
    case that: Set[T] => Set.Equiv[T].eq(this, that)
    case _ => false
  }

  //Symbolic aliases
  def ⊂(that: Set[T]) = this < that
  def ⊃(that: Set[T]) = this > that
  def ⊆(that: Set[T]) = this <= that
  def ⊇(that: Set[T]) = this >= that
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
    def elements = Iterable.empty
    def contains(x: T) = false
    override def |(that: Set[T]) = that
    override def \(that: Set[T]) = this
    override def &(that: Set[T]) = this
    override def <=(that: Set[T]) = true
    override def <(that: Set[T]) = that.size != 0
  }

  /** Returns the equivalence relation on sets. */
  implicit def Equiv[T]: Equiv[Set[T]] = new Equiv[Set[T]] {
    def eq(x: Set[T], y: Set[T]) = (x forall y) && (y forall x)
  }

  /** Returns the lattice on sets. */
  implicit def Lattice[T]: Lattice[Set[T]] with BoundedLowerSemilattice[Set[T]] =
    new Lattice[Set[T]] with BoundedLowerSemilattice[Set[T]] {
      def bot = empty[T]
      def inf(x: Set[T], y: Set[T]) = x & y
      def sup(x: Set[T], y: Set[T]) = x | y
  }

  def autoBuilder[T](implicit e: Equiv[T]): Builder[T, Set[T]] = e match {
    case e: IntHashing[T] => HashSet.newBuilder[T]
    case e: WeakOrder[T] => ???
    case _ => ???
  }
}

abstract class AbstractSet[T] extends Set[T]
