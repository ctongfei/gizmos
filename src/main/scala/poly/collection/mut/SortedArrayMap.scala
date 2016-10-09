package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.builder._
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

  override def pairs = Range(size) map { i => (keyArray(i), valArray(i)) } asIfSorted(keyOrder contramap first)

  def ?(k: K) = keyArray.binarySearch(k) map valArray

  def apply(k: K) = valArray(keyArray.binarySearch(k).get)

  def addInplace(k: K, v: V) = {
    val i = keyArray.lowerBound(k)
    keyArray.data.insertInplace(i, k)
    valArray.insertInplace(i, v)
  }

  def removeInplace(k: K) = for (i <- keyArray.binarySearch(k)) {
    keyArray.data.deleteInplace(i)
    valArray.deleteInplace(i)
  }

  def clear() = {
    keyArray.clear()
    valArray.clear()
  }

  def update(k: K, v: V) = {
    val i = keyArray.tryBinarySearch(k)
    if (i >= 0) valArray(i) = v
    else {
      keyArray.data.insertInplace(~i, k)
      valArray.insertInplace(~i, v)
    }
  }

}

object SortedArrayMap extends BuilderFactoryAB_EvA[SortedArrayMap, Order] {
  implicit def newBuilder[K: Order, V] = new Builder[(K, V), SortedArrayMap[K, V]] {
    private[this] val kva = ArraySeq[(K, V)]()
    def addInplace(x: (K, V)) = kva :+= x
    def result = {
      kva.sortInplace()(Order by first)
      val ka = ArraySeq[K]()
      val va = ArraySeq[V]()
      for ((k, v) <- kva) {
        ka :+= k
        va :+= v
      }
      new SortedArrayMap(new SortedArraySeq(ka.data), va.data)
    }
  }
}
