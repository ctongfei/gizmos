package poly.collection.impl

import poly.algebra._

/**
 * A key-value pair used in maps.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class KeyValuePair[K, V](val key: K, var value: V) extends Product2[K, V] {

  def _1 = key
  def _2 = value

  override def hashCode = key.##

  override def equals(that: Any) = that match {
    case that: KeyValuePair[K, V] => this.key == that.key
    case _ => false
  }

  def canEqual(that: Any) = that match {
    case that: Product2 => true
    case _ => false
  }
}

object KeyValuePair {

  /** Returns a hashing function on key-value pairs that operates only on the key. */
  implicit def hashByKey[K, V](implicit H: Hashing[K, Int]): Hashing[(K, V), Int] =
    new Hashing[(K, V), Int] {
      def hash(x: (K, V)) = H.hash(x._1)
      def eq(x: (K, V), y: (K, V)) = x._1 == y._1
    }

  /** Returns a weak order on key-value pairs that is based on the weak order on the key. */
  implicit def orderByKey[K, V](implicit O: WeakOrder[K]): WeakOrder[(K, V)] =
    new WeakOrder[(K, V)] {
      def cmp(x: (K, V), y: (K, V)) = O.cmp(x._1, y._1)
    }

  implicit def eqByKey[K, V](implicit E: Eq[K]): Eq[(K, V)] =
    new Eq[(K, V)] {
      def eq(x: (K, V), y: (K, V)) = E.eq(x._1, y._1)
    }
}
