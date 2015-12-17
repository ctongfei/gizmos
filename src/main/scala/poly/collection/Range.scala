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
 * @author Tongfei Chen
 * @since 0.1.0
 */
class Range private(
  private[this] val left: Int,
  private[this] val right: Int,
  private[this] val step: Int = 1
)
  extends SortedIndexedSeq[Int] {

  lazy val fastLength = {
    val gap = right - left
    val len = gap / step + (if (gap % step != 0) 1 else 0)
    if (len < 0) 0 else len
  }

  implicit def orderOnValue: TotalOrder[Int] = if (step > 0) TotalOrder[Int] else TotalOrder[Int].reverse

  def fastApply(i: Int): Int = left + i * step

  // Overridden for performance (rewrite to a while loop and then attempt to inline the loop body)
  override final def foreach[@sp(Unit) U](f: Int => U) = {
    if (step > 0)
      FastLoop.ascending(left, right, step)(f)
    else
      FastLoop.descending(left, right, step)(f)
  }

  // HELPER FUNCTIONS
  override def head = left

  override def tail = Range(left + step, right, step)

  override def reverse = Range(left + step * (length - 1), left - math.signum(step), -step)
}

object Range {

  /** Creates a left-inclusive-right-exclusive range [0, ''r''). */
  def apply(r: Int) = new Range(0, r)

  /** Creates a left-inclusive-right-exclusive range [''l'', ''r''). */
  def apply(l: Int, r: Int) = new Range(l, r)

  /** Creates a left-inclusive-right-exclusive range with the specific step size (can be negative). */
  def apply(l: Int, r: Int, step: Int) = new Range(l, r, step)

  /** Creates a closed range [0, ''r'']. */
  def inclusive(r: Int) = new Range(0, r + 1)

  /** Creates a closed range [''l'', ''r'']. */
  def inclusive(l: Int, r: Int) = new Range(l, r + 1)

  /** Creates a closed range with the specific step size (can be negative). */
  def inclusive(l: Int, r: Int, step: Int) = new Range(l, r + math.signum(step), step)


}
