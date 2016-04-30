package poly.collection.mut

import poly.algebra._
import poly.algebra.conversion.FromJava._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.FromJava._
import poly.collection.factory._

/**
 * Represents a set backed by a red-black tree. This class is a wrapper of [[java.util.TreeSet]].
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
class RedBlackTreeSet[K] private(private val data: java.util.TreeSet[K])
  extends AbstractSet[K] with BiSortedSet[K] with KeyMutableSet[K] { self =>

  def addInplace(x: K) = data.add(x)

  def clear() = data.clear()

  def removeInplace(x: K) = data.remove(x)

  def orderOnKeys = data.comparator()

  def keys = data.keys

  def contains(x: K) = data contains x

  override def size = data.size()

}

object RedBlackTreeSet extends SortedSetFactory[RedBlackTreeSet] {

  implicit def newBuilder[K](implicit K: WeakOrder[K]): Builder[K, RedBlackTreeSet[K]] =
    new Builder[K, RedBlackTreeSet[K]] {
      private[this] val data = new java.util.TreeSet[K](new java.util.Comparator[K] {
        def compare(a: K, b: K) = K.cmp(a, b)
      })
      def addInplace(x: K) = data.add(x)
      def result = new RedBlackTreeSet(data)
      def sizeHint(n: Int) = {}
    }
}
