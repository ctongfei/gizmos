package poly.collection

import poly.algebra._
import poly.algebra.ops._

/**
 * Basic trait for mutable indexed sequences.
 * O(1) random access and update should be guaranteed.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait MutIndexedSeq[T] extends MutSeq[T] with IndexedSeq[T] {

  /**
   * Swaps two elements in this sequence in-place.
   * @param i Index of the first element
   * @param j Index of the second element
   */
  def swap(i: Int, j: Int): Unit = {
    val t = this(i)
    this(i) = this(j)
    this(j) = t
  }

  /**
   * Sorts this sequence in-place using the order provided.
   * @param O The order for sorting
   */
  def inplaceSort()(implicit O: WeakOrder[T]): Unit = {
    //TODO: Change to introsort or timsort.
    def quicksort(i: Int, j: Int): Unit = {
      var l = i
      var r = j
      val pivot = this(l + (r - l) / 2)
      while (l <= r) {
        while (this(l) < pivot) l += 1
        while (this(r) > pivot) r -= 1
        if (l <= r) {
          swap(l, r)
          l += 1
          r -= 1
        }
      }
      if (i < r) quicksort(i, r)
      if (l < j) quicksort(l, j)
    }
    quicksort(0, length - 1)
  }

  /**
   * Reverses this sequence in-place.
   */
  def inplaceReverse(): Unit = {
    var l = 0
    var r = length - 1
    while (l <= r) {
      swap(l, r)
      l += 1
      r -= 1
    }
  }

  /**
   *  Transforms this sequence in-place given a function.
   *  @param f The function
   */
  def inplaceMap(f: T => T): Unit = {
    var i = 0
    while (i < length) {
      update(i, f(apply(i)))
      i += 1
    }
  }

}
