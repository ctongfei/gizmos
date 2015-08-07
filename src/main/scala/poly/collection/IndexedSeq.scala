package poly.collection

import poly.algebra._
import poly.collection.factory._
import poly.collection.mut._
import poly.collection.node._
import poly.util.specgroup._
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.reflect._

/**
 * Basic trait for indexed sequences. Indexed sequences should support efficient random access.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait IndexedSeq[+T] extends BiSeq[T] { self =>

  def length: Int

  def apply(i: Int): T

  // Overridden enumerator method for performance.
  override def newEnumerator: Enumerator[T] = new Enumerator[T] {
    private[this] var i: Int = -1
    def current = self(i)
    def advance(): Boolean = {
      i += 1
      i < self.length
    }
  }

  def headNode: BiSeqNode[T] = new IndexedSeqNode(0)
  def lastNode: BiSeqNode[T] = new IndexedSeqNode(length - 1)

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
  override def asIfSorted[U >: T](implicit O: WeakOrder[U]): IndexedSortedSeq[U] = new IndexedSortedSeq[U] {
    val order: WeakOrder[U] = O
    def length: Int = self.length
    def apply(i: Int): T = self.apply(i)
  }

  class IndexedSeqNode(val i: Int) extends BiSeqNode[T] {
    def data = self(i)
    def next = if (i == length - 1) null else new IndexedSeqNode(i + 1)
    def prev = if (i == 0) null else new IndexedSeqNode(i - 1)
  }

}

object IndexedSeq {

  object empty extends IndexedSeq[Nothing] {
    def apply(i: Int): Nothing = throw new NoSuchElementException
    def length: Int = 0
  }

  def tabulate[T](n: Int)(f: Int => T): IndexedSeq[T] = new IndexedSeq[T] {
    def length: Int = n
    def apply(i: Int): T = f(i)
  }

}
