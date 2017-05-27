package poly.collection.mut

import poly.collection._
import poly.collection.exception._
import poly.collection.typeclass._
import poly.collection.factory._
import poly.collection.impl._

/**
 * Represents a sorted map backed by two arrays: one sorted key array and one corresponding value array.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class SortedArrayMap[K, V] private(
  private[poly] val keyArray: SortedArraySeq[K],
  private[poly] val valArray: ResizableSeq[V]
) extends KeySortedMap[K, V] with KeyMutableMap[K, V] {

  def keySet: SortedSet[K] = new AbstractSortedSet[K] {
    def keyOrder = keyArray.elementOrder
    def keys = keyArray
    def contains(x: K) = keyArray contains x
  }

  override def size = keyArray.length

  override def pairs = Range(size) map { i => (keyArray(i), valArray(i)) } asIfSorted(keyOrder on first)

  def ?(k: K) = keyArray.binarySearch(k) match {
    case BinarySearchResult.Found(i) => Some(valArray(i))
    case _ => None
  }

  def apply(k: K) = keyArray.binarySearch(k) match {
    case BinarySearchResult.Found(i) => valArray(i)
    case _ => throw new KeyNotFoundException(k)
  }

  def add_!(k: K, v: V) = {
    val i = keyArray.indexOfLowerBound(k)
    keyArray.data.insert_!(i, k)
    valArray.insert_!(i, v)
  }

  def remove_!(k: K) = keyArray.binarySearch(k) match {
    case BinarySearchResult.Found(i) =>
      keyArray.data.delete_!(i)
      valArray.delete_!(i)
    case _ => /* do nothing */
  }

  def clear_!() = {
    keyArray.clear_!()
    valArray.clear_!()
  }

  def update(k: K, v: V) = {
    val i = keyArray.tryBinarySearch(k)
    if (i >= 0) valArray(i) = v
    else {
      keyArray.data.insert_!(~i, k)
      valArray.insert_!(~i, v)
    }
  }

}

object SortedArrayMap extends Factory2[Tuple2, SortedArrayMap, Order, Trivial.P1] {
  
  def newBuilder[K: Order, V: Trivial.P1] = new Builder[(K, V), SortedArrayMap[K, V]] {
    private[this] val kva = ArraySeq[(K, V)]()
    def add(x: (K, V)) = kva :+= x
    def result = {
      kva.sort_!()(Order by first)
      val (ka, va) = kva.unzipE
      new SortedArrayMap(new SortedArraySeq(ka.data), va.data)
    }
  }
}
