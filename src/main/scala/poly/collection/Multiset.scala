package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.algebra.specgroup._

/**
 * @author Tongfei Chen
 */
trait Multiset[@sp(i) K, R] extends KeyedLike[K, Multiset[K, R]] { self =>

  def equivOnKey: Equiv[K]

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

  def scale(w: R): Multiset[K, R] = new AbstractMultiset[K, R] {
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

  def multisetAdd(that: Multiset[K, R]): Multiset[K, R] = new AbstractMultiset[K, R] {
    def equivOnKey = self.equivOnKey
    implicit def ringOnCount = self.ringOnCount
    def multiplicity(k: K) = self.multiplicity(k) + that.multiplicity(k)
    def keys = self.keys union that.keys
    def containsKey(k: K) = self.containsKey(k) || that.containsKey(k)
  }

  def subsetOf(that: Multiset[K, R]) = this.keys forall { k => this.multiplicity(k) <= that.multiplicity(k) }

  def supersetOf(that: Multiset[K, R]) = that subsetOf this
}

object Multiset {

  def empty[K: Equiv, R: OrderedRing]: Multiset[K, R] = new Multiset[K, R] {
    def equivOnKey = Equiv[K]
    def multiplicity(k: K) = OrderedRing[R].zero
    def ringOnCount = OrderedRing[R]
    def keys = Iterable.empty
    def containsKey(k: K) = false
  }

  implicit def Module[K: Equiv, R: Ring]: Module[Multiset[K, R], R] = ???


}

abstract class AbstractMultiset[@sp(i) K, R] extends Multiset[K, R]
