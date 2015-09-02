package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.collection.exception._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait SortedIterable[T] extends Iterable[T] { self =>

  /** The order under which this sequence is sorted. */
  implicit def order: WeakOrder[T]

  /**
   * Merges two sorted sequences into one sorted sequence. $LAZY
   * @param that Another sorted sequence. These two sequences must be sorted under the same order.
   * @return A merged sorted sequence
   * @throws IncompatibleOrderException If two sequences are not sorted under the same order.
   */
  def merge(that: SortedIterable[T]): SortedIterable[T] = new SortedIterable[T] {
    if (this.order ne that.order) throw new IncompatibleOrderException
    implicit def order: WeakOrder[T] = self.order
    def newIterator: Iterator[T] = new AbstractIterator[T] {
      val ai = self.newIterator
      val bi = that.newIterator
      var curr: T = _
      var aNotComplete = ai.advance()
      var bNotComplete = bi.advance()
      def advance(): Boolean = {
        if (aNotComplete && bNotComplete) {
          if (ai.current <= bi.current) {
            curr = ai.current
            aNotComplete = ai.advance()
            aNotComplete
          } else {
            curr = bi.current
            bNotComplete = bi.advance()
            bNotComplete
          }
        }
        else if (aNotComplete) {
          curr = ai.current
          ai.advance()
        }
        else if (bNotComplete) {
          curr = bi.current
          bi.advance()
        }
        else false
      }

      def current: T = curr
    }
  }

  def merge(that: SortedSeq[T]): SortedSeq[T] = {
    val ai = this.newIterator
    val bi = that.newIterator
    val c = ArraySeq[T]()
    var aNotComplete = ai.advance()
    var bNotComplete = bi.advance()
    while (aNotComplete && bNotComplete) {
      if (ai.current <= bi.current) {
        c.appendInplace(ai.current)
        aNotComplete = ai.advance()
      } else {
        c.appendInplace(bi.current)
        bNotComplete = bi.advance()
      }
    }

    // Appends remaining elements
    if (aNotComplete) do c.appendInplace(ai.current) while (ai.advance())
    if (bNotComplete) do c.appendInplace(bi.current) while (bi.advance())
    c.asIfSorted(this.order)
  }

}
