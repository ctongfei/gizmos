package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.builder._
import poly.collection.conversion.Java._
import poly.collection.factory._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class HashSet[T] private(val data: java.util.HashSet[T]) extends MutableSet[T] with HasKnownSize {

  def equivOnKey = Equiv.default[T]

  def add(x: T) = data.add(x)

  def remove(x: T) = data.remove(x)

  def contains(x: T) = data.contains(x)

  def elements = data

  override def size: Int = data.size
}

object HashSet extends CollectionFactory[HashSet] {

  implicit def newBuilder[T]: Builder[T, HashSet[T]] = new Builder[T, HashSet[T]] {
    private[this] var js = new java.util.HashSet[T]()
    def +=(x: T) = js.add(x)
    def result = new HashSet[T](js)
    def sizeHint(n: Int) = js = new java.util.HashSet[T](n)
  }
}
