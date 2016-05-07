package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection._
import poly.macroutil._

import scala.util._

/**
 * Represents a mutable indexed sequence.
 * Fast random access and update should be guaranteed.
 * @since 0.1.0
 * @author Tongfei Chen
 */
trait ValueMutableIndexedSeq[T] extends ValueMutableSeq[T] with IndexedSeq[T] {

  /**
   * Sorts this sequence in-place using the order provided.
   * @param T The order for sorting
   */
  def sortInplace()(implicit T: Order[T]): Unit = {
    def quicksort(i: Int, j: Int): Unit = {
      var l = i
      var r = j
      val pivot = this(l + (r - l) / 2)
      while (l <= r) {
        while (this(l) < pivot) l += 1
        while (this(r) > pivot) r -= 1
        if (l <= r) {
          swapInplace(l, r)
          l += 1
          r -= 1
        }
      }
      if (i < r) quicksort(i, r)
      if (l < j) quicksort(l, j)
    }
    quicksort(0, length - 1)
  }

  def sortInplaceUsing[U: Order](w: IndexedSeq[U]): Unit = {
    def quicksort(i: Int, j: Int): Unit = {
      var l = i
      var r = j
      val pivot = w(l + (r - l) / 2)
      while (l <= r) {
        while (w(l) < pivot) l += 1
        while (w(r) > pivot) r -= 1
        if (l <= r) {
          swapInplace(l, r)
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
  def reverseInplace(): Unit = {
    var l = 0
    var r = length - 1
    while (l <= r) {
      swapInplace(l, r)
      l += 1
      r -= 1
    }
  }

  /**
   *  Transforms this sequence in-place given a function.
   *  @param f The function
   */
  override def mapInplace(f: T => T): Unit = {
    var i = 0
    while (i < length) {
      this(i) = f(this(i))
      i += 1
    }
  }

  /** Randomly shuffles this sequence in-place using the Fisher-Yates shuffling algorithm. */
  def shuffleInplace(): Unit = {
    val r = new Random()
    FastLoop.descending(length - 1, 0, -1) { i =>
      val j = r.nextInt(i + 1)
      swapInplace(i, j)
    }
  }

}
