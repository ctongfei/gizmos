package poly.collection.impl

import poly.algebra._
import poly.algebra.specgroup._
import poly.algebra.ops._

/**
 * A key-value pair used in maps.
 * The key in a pair is immutable; whereas the value is mutable.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
private[poly] class KeyValuePair[@sp(i) K, V](val key: K, var value: V) extends Product2[K, V] {

  def _1 = key
  def _2 = value

  def toTuple = (key, value)

  override def hashCode = key.##

  override def equals(that: Any) = that match {
    case that: KeyValuePair[K, V] => this.key == that.key
    case _ => false
  }

  def canEqual(that: Any) = that match {
    case that: Product2[_, _] => true
    case _ => false
  }
}

object KeyValuePair {

  def apply[@sp(i) K, V](key: K, value: V) = new KeyValuePair(key, value)

  /** Returns a hashing function on key-value pairs that operates only on the key. */
  implicit def hashByKey[@sp(i) K, V](implicit K: IntHashing[K]): IntHashing[KeyValuePair[K, V]] =
    new IntHashing[KeyValuePair[K, V]] {
      def hash(x: KeyValuePair[K, V]) = K.hash(x.key)
      def eq(x: KeyValuePair[K, V], y: KeyValuePair[K, V]) = x.key =~= y.key
    }

  /** Returns a weak order on key-value pairs that is based on the weak order on the key. */
  implicit def orderByKey[@sp(i) K: WeakOrder, V]: WeakOrder[KeyValuePair[K, V]] =
    new WeakOrder[KeyValuePair[K, V]] {
      def cmp(x: KeyValuePair[K, V], y: KeyValuePair[K, V]) = x.key >?< y.key
    }

  /** Returns an equivalence relation on key-value pairs that is based on the equivalence relation on the key. */
  implicit def equivByKey[@sp(i) K: Equiv, V]: Equiv[KeyValuePair[K, V]] =
    new Equiv[KeyValuePair[K, V]] {
      def eq(x: KeyValuePair[K, V], y: KeyValuePair[K, V]) = x.key =~= y.key
    }
}
