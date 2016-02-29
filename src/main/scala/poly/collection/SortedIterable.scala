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
trait SortedIterable[T] extends Iterable[T] { self =>

  /** The order under which the elements of this collection is sorted. */
  implicit def order: WeakOrder[T]

  override def filter(f: T => Boolean): SortedIterable[T] = Iterable.ofIterator {
    new Iterator[T] {
      private[this] val i = self.newIterator
      def current: T = i.current
      def advance(): Boolean = {
        do {
          val hasNext = i.advance()
          if (!hasNext) return false
        } while (!f(i.current))
        true
      }
    }
  }.asIfSorted(order)

  override def filterNot(f: T => Boolean) = filter(x => !f(x))

  /**
   * Merges two sorted iterable collection into one sorted iterable collection. $LAZY
   * @param that Another sorted sequence. These two sequences must be sorted under the same order.
   * @return A merged sorted sequence
   */
  def merge(that: SortedIterable[T]): SortedIterable[T] = new SortedIterable[T] {
    implicit def order: WeakOrder[T] = self.order
    def newIterator: Iterator[T] = new Iterator[T] {
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

  //TODO: delete?
  @deprecated
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
