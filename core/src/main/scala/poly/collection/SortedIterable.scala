package poly.collection

import cats.implicits._
import poly.collection.mut._

/**
 * Represents an iterable collection that is sorted according to a specific order every time it is iterated.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait SortedIterable[T] extends Iterable[T] { self =>

  /** Returns the order under which the elements of this collection are sorted. */
  implicit def elementOrder: Order[T]

  override def filter(f: T => Boolean): SortedIterable[T] = super.filter(f) asIfSorted elementOrder

  override def filterNot(f: T => Boolean) = filter(!f)

  /**
   * Returns the unique elements of this iterable collection while retaining their original order.
   * The equivalence function is this sorted iterable collection's inherent order.
   */
  def distinct: SortedIterable[T] = new SortedIterableT.Distinct(self)

  /**
   * Merges two sorted iterable collection into one sorted iterable collection. $LAZY
   * @param that Another sorted sequence. These two sequences must be sorted under the same order.
   * @return A merged sorted sequence
   */
  def merge(that: SortedIterable[T]): SortedIterable[T] = new SortedIterableT.Merged(self, that)

  def min = self.head

  def max = self.last

  /** Merges two sorted iterables eagerly into a sorted sequence. */
  def mergeE(that: SortedIterable[T]): SortedSeq[T] = {
    val ai = this.newIterator
    val bi = that.newIterator
    val c = ArraySeq[T]()
    var aNotComplete = ai.advance()
    var bNotComplete = bi.advance()
    while (aNotComplete && bNotComplete) {
      if (ai.current <= bi.current) {
        c :+= ai.current
        aNotComplete = ai.advance()
      } else {
        c :+= bi.current
        bNotComplete = bi.advance()
      }
    }

    // Appends remaining elements
    if (aNotComplete) do c :+= ai.current while (ai.advance())
    if (bNotComplete) do c :+= bi.current while (bi.advance())
    c.asIfSorted(this.elementOrder)
  }

}

abstract class AbstractSortedIterable[T] extends AbstractIterable[T] with SortedIterable[T]

private[poly] object SortedIterableT {

  class Distinct[T](self: SortedIterable[T]) extends AbstractSortedIterable[T] {
    implicit def elementOrder = self.elementOrder
    def newIterator = new AbstractIterator[T] {
      private[this] val it = self.newIterator
      private[this] var curr = default[T]
      private[this] var first = true
      def current = curr
      def advance(): Boolean = {
        while (it.advance()) {
          if (first || (it.current =!= curr)) {
            first = false
            curr = it.current
            return true
          }
        }
        false
      }
    }
  }

  class Merged[T](self: SortedIterable[T], that: SortedIterable[T]) extends AbstractSortedIterable[T] {
    implicit def elementOrder: Order[T] = self.elementOrder
    def newIterator: Iterator[T] = new AbstractIterator[T] {
      private[this] val ai = self.newIterator
      private[this] val bi = that.newIterator
      private[this] var curr: T = _
      private[this] var aNotComplete = ai.advance()
      private[this] var bNotComplete = bi.advance()
      def advance() = {
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

      def current = curr
    }
  }

}