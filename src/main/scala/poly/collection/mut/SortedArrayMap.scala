package poly.collection.mut

import poly.collection._
import poly.collection.exception._

/**
 * @author Tongfei Chen
 */
class SortedArrayMap[K, V] private(private val data: SortedArraySeq[(K, V)]) extends SortedMap[K, V] {

  def keySet = ???

  def ?(k: K) = ???

  def apply(k: K) = data.binarySearch((k, default[V])) match {
    case Some(i) => data(i)._2
    case None => throw new KeyNotFoundException(k)
  }

}