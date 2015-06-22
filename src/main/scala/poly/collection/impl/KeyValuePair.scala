package poly.collection.impl

import poly.algebra._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
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
  
}
