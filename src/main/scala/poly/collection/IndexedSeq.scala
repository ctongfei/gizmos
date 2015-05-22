package poly.collection

import poly.algebra._
import scala.annotation.unchecked.{uncheckedVariance => uV}

/**
 * Basic trait for indexed sequences. Indexed sequences should support O(1) random access.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait IndexedSeq[+T] extends Seq[T] { self =>

  def apply(i: Int): T

  def enumerator: Enumerator[T] = new Enumerator[T] {
    private[this] var i: Int = -1
    def current = self(i)
    def advance(): Boolean = {
      i += 1
      i < self.size
    }
  }

  // HELPER FUNCTIONS

  /**
   * Rotates this sequence from the index specified.
   * @param j Rotation starts here
   * @return
   */
  def rotate(j: Int): IndexedSeq[T] = new IndexedSeq[T] {
    val len = self.length
    def apply(i: Int): T = self((j + i) % len)
    def length: Int = len
  }

  /**
   * Pretends that this sequence is sorted under the given order.
   * (WARNING: Actual orderedness is not guaranteed! The user should make sure that it is sorted.)
   * @param O The implicit order
   * @return A sorted order
   */
  override def asIfSorted(implicit O: WeakOrder[T @uV]): SortedIndexedSeq[T @uV] = new SortedIndexedSeq[T] {
    val order: WeakOrder[T] = O
    def length: Int = self.length
    def apply(i: Int): T = self.apply(i)
  }

}
