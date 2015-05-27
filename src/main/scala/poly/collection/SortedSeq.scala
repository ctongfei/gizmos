package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.collection.exception._
import poly.collection.mut._

import scala.reflect._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait SortedSeq[T] extends Seq[T] {

  /** The order under which this sequence is sorted. */
  implicit val order: WeakOrder[T]

  /**
   * Merges two sorted sequences into one sorted sequence.
   * @param that Another sorted sequence. These two sequences must be sorted under the same order.
   * @return A merged sorted sequence
   * @throws IncompatibleOrderException If two sequences are not sorted under the same order.
   */
  def merge(that: SortedSeq[T])(implicit tag: ClassTag[T]): SortedSeq[T] = {
    if (this.order ne that.order) throw new IncompatibleOrderException
    val ai = this.enumerator
    val bi = that.enumerator
    val c = ArraySeq[T]()
    var aNotComplete = ai.advance()
    var bNotComplete = bi.advance()
    while (aNotComplete && bNotComplete) {
      if (ai.current <= bi.current) {
        c.append(ai.current)
        aNotComplete = ai.advance()
      } else {
        c.append(bi.current)
        bNotComplete = bi.advance()
      }
    }

    // Appends remaining elements
    if (aNotComplete) do c.append(ai.current) while (ai.advance())
    if (bNotComplete) do c.append(bi.current) while (bi.advance())
    c.asIfSorted(this.order)
  }

  def distinct(implicit tag: ClassTag[T]): SortedSeq[T] = ???

}
