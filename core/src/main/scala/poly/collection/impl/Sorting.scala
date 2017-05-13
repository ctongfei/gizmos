package poly.collection.impl

import cats.implicits._
import poly.collection._
import poly.collection.mut._

/**
 * Contains common sorting algorithms.
 * @author Tongfei Chen
 */
object Sorting {

  def quickSort[T, U >: T](a: ValueMutableIndexedSeq[T])(implicit U: Order[U]): Unit = {
    implicit val T: Order[T] = U.on(identity)
    def quicksort0(i: Int, j: Int): Unit = {
      var l = i
      var r = j
      val pivot = a(l + (r - l) / 2)
      while (l <= r) {
        while (a(l) < pivot) l += 1
        while (a(r) > pivot) r -= 1
        if (l <= r) {
          a.swap_!(l, r)
          l += 1
          r -= 1
        }
      }
      if (i < r) quicksort0(i, r)
      if (l < j) quicksort0(l, j)
    }
    quicksort0(0, a. length - 1)
  }

  def quickSortUsing[T, U: Order](a: ValueMutableIndexedSeq[T], w: IndexedSeq[U]): Unit = {
    require(a.length == w.length)
    def quicksort0(i: Int, j: Int): Unit = {
      var l = i
      var r = j
      val pivot = w(l + (r - l) / 2)
      while (l <= r) {
        while (w(l) < pivot) l += 1
        while (w(r) > pivot) r -= 1
        if (l <= r) {
          a.swap_!(l, r)
          l += 1
          r -= 1
        }
      }
      if (i < r) quicksort0(i, r)
      if (l < j) quicksort0(l, j)
    }
    quicksort0(0, a.length - 1)
  }

}
