package poly.collection.mut

import poly.algebra._
import poly.algebra.syntax._
import poly.collection._
import poly.collection.impl._
import poly.macroutil._

/**
 * Represents a mutable indexed sequence.
 * Fast random access and update should be guaranteed.
 * @since 0.1.0
 * @author Tongfei Chen
 */
trait ValueMutableIndexedSeq[T] extends ValueMutableSeq[T] with IndexedSeq[T] {

  /** Sorts this sequence in-place using the order provided. */
  def sort_!()(implicit T: Order[T]) = Sorting.quickSort(this)

  /** Sorts this sequence in-place using the weight provided in the given indexed sequence. */
  def sortUsing_![U: Order](w: IndexedSeq[U]) = Sorting.quickSortUsing(this, w)

  /** Reverses this sequence in-place. */
  def reverse_!(): Unit = {
    var l = 0
    var r = length - 1
    while (l <= r) {
      swap_!(l, r)
      l += 1
      r -= 1
    }
  }

  /** Transforms this sequence in-place given a function. */
  override def map_!(f: T => T): Unit = {
    FastLoop.ascending(0, length, 1) { i =>
      this(i) = f(this(i))
    }
  }

  /** Randomly shuffles this sequence in-place using the Fisher-Yates shuffling algorithm. */
  def shuffle_!(): Unit = {
    val r = new java.util.Random()
    FastLoop.descending(length - 1, 0, -1) { i =>
      val j = r.nextInt(i + 1)
      swap_!(i, j)
    }
  }

}
