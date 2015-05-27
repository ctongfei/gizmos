package poly.collection.mut

import poly.collection._
import poly.collection.impl._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
abstract class SortedArraySet[T] private(private val data: SortedArray[T]) extends Set[T] {

  val order = data.order



}
