package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._

/**
 * A multiset in which elements can appear more than once.
 *
 * The multiplicity in Poly-collection's Multiset can be real-valued:
 * as long as it forms an ordered ring, it can be used as a type for the
 * counts for the elements.
 *
 * Multisets are useful for implementing counters.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Multiset[@sp(i) K, R] extends KeyedLike[K, Multiset[K, R]] { self =>

  implicit def equivOnKey: Equiv[K]

  /** Returns the ring structure endowed on the counts of this multiset. */
  implicit def ringOnCount: OrderedRing[R]

  def containsKey(k: K): Boolean

  def multiplicity(k: K): R

  def keys: Iterable[K]

  def pairs: Iterable[(K, R)] = keys.map(k => k → multiplicity(k))

  final def apply(k: K): R = multiplicity(k)

  def filterKeys(f: K => Boolean): Multiset[K, R] = new AbstractMultiset[K, R] {
    def equivOnKey = self.equivOnKey
    override def pairs  = self.pairs.filter { case (k, r) => f(k) }
    def multiplicity(k: K) = if (f(k)) self.multiplicity(k) else zero[R]
    implicit def ringOnCount = self.ringOnCount
    def keys = self.keys.filter(f)
    def containsKey(k: K) = f(k) && self.containsKey(k)
  }

  def keySet: Set[K] = new AbstractSet[K] {
    def equivOnKey = self.equivOnKey
    def keys = self.keys
    def contains(k: K) = self.containsKey(k)
  }

  def scaleBy(w: R): Multiset[K, R] = new AbstractMultiset[K, R] {
    def equivOnKey = self.equivOnKey
    implicit def ringOnCount = self.ringOnCount
    override def pairs = self.pairs.map { case (k, r) => k → (r * w) }
    def multiplicity(k: K) = self.multiplicity(k) * w
    def keys = self.keys
    def containsKey(k: K) = self.containsKey(k)
  }

  def intersect(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    def equivOnKey = self.equivOnKey
    implicit def ringOnCount = self.ringOnCount
    def multiplicity(k: K) = min(self.multiplicity(k), that.multiplicity(k))
    def keys = self.keys intersect that.keys
    def containsKey(k: K) = self.containsKey(k) && that.containsKey(k)
  }

  def union(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    def equivOnKey = self.equivOnKey
    implicit def ringOnCount = self.ringOnCount
    def multiplicity(k: K) = max(self.multiplicity(k), that.multiplicity(k))
    def keys = self.keys union that.keys
    def containsKey(k: K) = self.containsKey(k) || that.containsKey(k)
  }

  def product[L](that: Multiset[L, R]): Multiset[(K, L), R] = new AbstractMultiset[(K, L), R] {
    def equivOnKey = Equiv.product(self.equivOnKey, that.equivOnKey)
    implicit def ringOnCount = self.ringOnCount
    def multiplicity(k: (K, L)) = self.multiplicity(k._1) * that.multiplicity(k._2)
    def keys = self.keys product that.keys
    def containsKey(k: (K, L)) = self.containsKey(k._1) && that.containsKey(k._2)
  }

  def multisetAdd(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    def equivOnKey = self.equivOnKey
    implicit def ringOnCount = self.ringOnCount
    def multiplicity(k: K) = self.multiplicity(k) + that.multiplicity(k)
    def keys = self.keys union that.keys
    def containsKey(k: K) = self.containsKey(k) || that.containsKey(k)
  }

  def subsetOf(that: Multiset[K, R]) = this.keys forall { k => this.multiplicity(k) <= that.multiplicity(k) }

  def supersetOf(that: Multiset[K, R]) = that subsetOf this

  def :*(w: R): Multiset[K, R] = scaleBy(w)
  def *:(w: R): Multiset[K, R] = scaleBy(w)


}

object Multiset {

  def empty[K: Equiv, R: OrderedRing]: Multiset[K, R] = new Multiset[K, R] {
    def equivOnKey = Equiv[K]
    def multiplicity(k: K) = OrderedRing[R].zero
    def ringOnCount = OrderedRing[R]
    def keys = Iterable.empty
    def containsKey(k: K) = false
  }

  /** Returns the implicit module structure on multisets. */
  implicit def Module[K: Equiv, R: OrderedRing]: Module[Multiset[K, R], R] = new Module[Multiset[K, R], R] {
    implicit def ringOnScalar = Ring[R]
    def scale(x: Multiset[K, R], k: R) = x scaleBy k
    def add(x: Multiset[K, R], y: Multiset[K, R]) = x multisetAdd y
    def zero = Multiset.empty[K, R]
  }

  implicit def Lattice[K: Equiv, R: OrderedRing]: Lattice[Multiset[K, R]] = new Lattice[Multiset[K, R]] {
    def sup(x: Multiset[K, R], y: Multiset[K, R]) = x union y
    def inf(x: Multiset[K, R], y: Multiset[K, R]) = x intersect y
  }


}

abstract class AbstractMultiset[@sp(i) K, R] extends Multiset[K, R]
