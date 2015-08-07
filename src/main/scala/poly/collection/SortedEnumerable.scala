package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.collection.exception._
import poly.collection.mut._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait SortedEnumerable[T] extends Enumerable[T] { self =>

  /** The order under which this sequence is sorted. */
  implicit def order: WeakOrder[T]

  /**
   * Merges two sorted sequences into one sorted sequence. $LAZY
   * @param that Another sorted sequence. These two sequences must be sorted under the same order.
   * @return A merged sorted sequence
   * @throws IncompatibleOrderException If two sequences are not sorted under the same order.
   */
  def merge(that: SortedEnumerable[T]): SortedEnumerable[T] = new SortedEnumerable[T] {
    if (this.order ne that.order) throw new IncompatibleOrderException
    implicit def order: WeakOrder[T] = self.order
    def newEnumerator: Enumerator[T] = new Enumerator[T] {
      val ai = self.newEnumerator
      val bi = that.newEnumerator
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
    val ai = this.newEnumerator
    val bi = that.newEnumerator
    val c = ArraySeq[T]()
    var aNotComplete = ai.advance()
    var bNotComplete = bi.advance()
    while (aNotComplete && bNotComplete) {
      if (ai.current <= bi.current) {
        c.inplaceAppend(ai.current)
        aNotComplete = ai.advance()
      } else {
        c.inplaceAppend(bi.current)
        bNotComplete = bi.advance()
      }
    }

    // Appends remaining elements
    if (aNotComplete) do c.inplaceAppend(ai.current) while (ai.advance())
    if (bNotComplete) do c.inplaceAppend(bi.current) while (bi.advance())
    c.asIfSorted(this.order)
  }

}
