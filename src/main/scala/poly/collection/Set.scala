package poly.collection

import poly.algebra._
import poly.collection.mut._

/**
 * Basic trait for sets whose elements can be enumerated.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Set[T] extends PredicateSet[T] with Multiset[T] { self =>

  // ListSet: Equiv[T]
  // HashSet: IntHashing[T]
  // TreeSet: WeakOrder[T]
  def equivOnKey: Equiv[T]

  override def distinct = self

  def multiplicity(x: T) = if (contains(x)) 1 else 0

  /** Tests if an element belongs to this set. */
  def contains(x: T): Boolean

  final override def keys = elements

  /** Tests if an element does not belong to this set. */
  def notContains(x: T) = !contains(x)

  /** Returns the union of two sets. */
  def |(that: Set[T]): Set[T] = {
    require(self.equivOnKey equivSameAs that.equivOnKey)
    new AbstractSet[T] {
      def equivOnKey = self.equivOnKey
      def contains(x: T) = self.contains(x) && that.contains(x)
      def size = elements.size
      def elements = self.elements ++ that.elements.filter(self.notContains)
    }
  }

  /** Returns the intersection of two sets. */
  def &(that: Set[T]): Set[T] = {
    require(self.equivOnKey equivSameAs that.equivOnKey)
    new AbstractSet[T] {
      def equivOnKey = self.equivOnKey
      def contains(x: T) = self.contains(x) && that.contains(x)
      def size = elements.size
      def elements = self.elements.filter(that.contains)
    }
  }

  /** Returns the difference of two sets. */
  def &~(that: Set[T]): Set[T] = {
    require(self.equivOnKey equivSameAs that.equivOnKey)
    new AbstractSet[T] {
      def equivOnKey: Equiv[T] = self.equivOnKey
      def contains(x: T): Boolean = self.contains(x) && that.notContains(x)
      def size = elements.size
      def elements = self.elements.filter(that.notContains)
    }
  }

  /** Tests if this set is a subset of another set. */
  def <=(that: Set[T]): Boolean = this.forall(that.contains)

  /** Tests if this set is a strict subset of another set. */
  def <(that: Set[T]): Boolean = this.forall(that.contains) && that.exists(this.notContains)

  /** Tests if this set is a strict superset of another set. */
  def >(that: Set[T]): Boolean = that < this

  /** Tests if this set is a superset of another set. */
  def >=(that: Set[T]): Boolean = that <= this

  def filterKeys(f: T => Boolean): Set[T] = new AbstractSet[T] {
    def equivOnKey = self.equivOnKey
    def contains(x: T) = self.contains(x) && f(x)
    def size = elements.size
    def elements = self.elements.filter(f)
  }

  def mapKeys[U](f: T => U)(implicit U: Equiv[U]): Set[U] = {
    self.elements.map(f).to(Set.autoBuilder(U))
  }

  override def equals(that: Any) = that match {
    case that: Set[T] => this.forall(that.contains) && that.forall(this.contains)
    case that: Multiset[T] => that.equals(this)
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
    override def &~(that: Set[T]) = this
    override def &(that: Set[T]) = this
    override def <=(that: Set[T]) = true
    override def <(that: Set[T]) = that.size != 0
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
