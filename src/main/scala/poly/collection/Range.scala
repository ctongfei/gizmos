package poly.collection

import poly.algebra._
import poly.algebra.implicits._
import poly.collection.exception._
import poly.util.fastloop._
import poly.util.specgroup._

/**
 * Represents an immutable integer range.
 *
 * The difference between this class and [[scala.collection.immutable.Range]] is that
 * [[poly.collection.Range]] will attempt to inline the loop body when iterating over
 * the range using macros, which potentially makes it more efficient.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
class Range private(val left: Int, val right: Int, val step: Int = 1) extends IndexedSortedSeq[Int] {

  // Ensures that this is a valid range
  require((step > 0 && left < right) || (step < 0 && left > right))

  val length = if (step > 0) (right - left) / step else (left - right) / step

  implicit def order: WeakOrder[Int] = if (step > 0) WeakOrder[Int] else WeakOrder[Int].reverse

  def apply(i: Int): Int = {
    if (i < 0 || i >= length) throw new NoSuchElementException
    left + i * step
  }

  // Overridden for performance (rewrite to a while loop and then attempt to inline the loop body)
  override final def foreach[@sp(Unit) U](f: Int => U) = {
    if (step > 0)
      FastLoop.ascending(left, right, step)(f)
    else
      FastLoop.descending(left, right, step)(f)
  }

  override def reverse = Range(left + step * (length - 1), left - 1, -step)
}

object Range {

  def apply(r: Int) = new Range(0, r)
  def apply(l: Int, r: Int) = new Range(l, r)
  def apply(l: Int, r: Int, step: Int) = new Range(l, r, step)


}
