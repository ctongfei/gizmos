package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.collection.exception._
import poly.collection.mut._

/**
 * Represents an iterable collection that is sorted according to a specific order every time it is iterated.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait SortedIterable[T] extends Iterable[T] { self ⇒

  /** Returns the order under which the elements of this collection are sorted. */
  implicit def orderOnElements: Order[T]

  override def filter(f: T ⇒ Boolean): SortedIterable[T] = super.filter(f).asIfSorted(orderOnElements)

  override def filterNot(f: T ⇒ Boolean) = filter(x ⇒ !f(x))

  /**
   * Returns the unique elements of this iterable collection while retaining their original order.
   * The equivalence function is this sorted iterable collection's inherent order.
   */
  def distinct: Iterable[T] = self.distinct(orderOnElements)

  /**
   * Merges two sorted iterable collection into one sorted iterable collection. $LAZY
   * @param that Another sorted sequence. These two sequences must be sorted under the same order.
   * @return A merged sorted sequence
   */
  def merge(that: SortedIterable[T]): SortedIterable[T] = new SortedIterable[T] {
    implicit def orderOnElements: Order[T] = self.orderOnElements
    def newIterator: Iterator[T] = new AbstractIterator[T] {
      private[this] val ai = self.newIterator
      private[this] val bi = that.newIterator
      private[this] var curr: T = _
      private[this] var aNotComplete = ai.advance()
      private[this] var bNotComplete = bi.advance()
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

  def min = self.head

  def max = self.last

  def mergeEagerly(that: SortedIterable[T]): SortedSeq[T] = {
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
    c.asIfSorted(this.orderOnElements)
  }

}

abstract class AbstractSortedIterable[T] extends AbstractIterable[T] with SortedIterable[T]
