package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.impl._

/**
 * @author Tongfei Chen
 */
class HashSet[T: IntHashing] private(val data: ClosedHashingSet[T]) extends MutableSet[T] with HasKnownSize {

  def equivOnKey = implicitly[IntHashing[T]]

  def add(x: T) = data.insert(x)

  def remove(x: T) = data.remove(x)

  def contains(x: T) = data.locate(x) != -1

  def keys = Iterable.ofIterator {
    new Iterator[T] {
      private[this] var i = -1
      def current = data.keys(i).asInstanceOf[T]
      def advance(): Boolean = {
        while (i < data.capacity) {
          i += 1
          if (i < data.capacity && data.stat(i) == ClosedHashingSet.INUSE) return true
        }
        false
      }
    }
  }

  override def size: Int = data.size
}

object HashSet {

  def apply[T](): HashSet[T] = new HashSet[T](new ClosedHashingSet[T]())

  implicit def newBuilder[T: IntHashing]: Builder[T, HashSet[T]] = new Builder[T, HashSet[T]] {
    private[this] var s = new ClosedHashingSet[T]()
    def add(x: T) = s.insert(x)
    def result = new HashSet[T](s)
    def sizeHint(n: Int) = s = new ClosedHashingSet[T](n)
  }
}
