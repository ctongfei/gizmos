package poly.collection

import poly.algebra._
import poly.collection.factory._
import poly.collection.mut._
import poly.collection.node._
import poly.util.specgroup._
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.reflect._

/**
 * Basic trait for indexed sequences.
 *
 * Indexed sequences should support efficient random access (typically O(1)).
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait IndexedSeq[+T] extends BiSeq[T] { self =>

  override def hasKnownSize = true

  def length: Int

  def apply(i: Int): T

  // Overridden enumerator method for performance.
  override def newIterator: Iterator[T] = new AbstractIterator[T] {
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

  override def map[U](f: T => U): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def length = self.length
    def apply(i: Int) = f(self(i))
  }

  /**
   * Returns the Cartesian product of two indexed sequences. The returning value is a table.
   */
  def cartesianProduct[U](that: IndexedSeq[U]): Table[(T, U)] = new AbstractTable[(T, U)] {
    def apply(i: Int, j: Int) = (self(i), that(j))
    def numRows = self.length
    def numCols = that.length
  }

  def concat[U >: T](that: IndexedSeq[U]): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def length = self.length + that.length
    def apply(i: Int) = if (i < self.length) self(i) else that(i - self.length)
  }

  override def prepend[U >: T](x: U): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def length = self.length + 1
    def apply(i: Int) = if (i == 0) x else self(i - 1)
  }

  override def append[U >: T](x: U): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def length = self.length + 1
    def apply(i: Int) = if (i == self.length) x else self(i)
  }



  /**
   * Rotates this sequence from the index specified.
   * @param j Rotation starts here
   * @return
   */
  def rotate(j: Int): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    val len = self.length
    def apply(i: Int): T = self((j + i) % len)
    def length: Int = len
  }


  override def slice(i: Int, j: Int) = new AbstractIndexedSeq[T] {
    def apply(n: Int) = self(i + n)
    def length = j - i
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

  def interleave[U >: T](that: IndexedSeq[U]): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def length = math.min(this.length, that.length) * 2
    def apply(i: Int) = if (i % 2 == 0) self(i / 2) else that(i / 2)
  }

  override def sliding(windowSize: Int, step: Int = 1): IndexedSeq[IndexedSeq[T]] = new AbstractIndexedSeq[IndexedSeq[T]] {
    val length = (self.length - windowSize) / step + 1
    def apply(i: Int) = self.slice(step * i, step * i + windowSize)
  }

  // HELPER OBJECTS / CLASSES

  case class IndexedSeqNode(i: Int) extends BiSeqNode[T] {
    def data = self(i)
    def next = if (i == length - 1) Dummy else new IndexedSeqNode(i + 1)
    def prev = if (i == 0) Dummy else new IndexedSeqNode(i - 1)
  }

  private object Dummy extends BiSeqNode[Nothing] {
    def next: BiSeqNode[Nothing] = this
    def prev: BiSeqNode[Nothing] = this
    def data: Nothing = throw new NoSuchElementException
    override def isDummy = true
    override def reverse = this
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

abstract class AbstractIndexedSeq[+T] extends IndexedSeq[T]