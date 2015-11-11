package poly.collection

import poly.algebra._
import poly.collection.factory._
import poly.collection.mut._
import poly.collection.node._
import poly.util.fastloop._
import poly.util.specgroup._
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.reflect._

/**
 * Represents an indexed sequence.
 *
 * Indexed sequences should support efficient random access (typically O(1), sometimes may be O(log ''n'')).
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait IndexedSeq[+T] extends BiSeq[T] with HasKnownSize { self =>

  def fastLength: Int

  def fastApply(i: Int): T

  override def length = fastLength

  override def apply(i: Int) = fastApply(i)

  // Overridden newIterator method for performance.
  override def newIterator: Iterator[T] = new Iterator[T] {
    private[this] var i: Int = -1
    def current = self(i)
    def advance(): Boolean = {
      i += 1
      i < self.fastLength
    }
  }

  def headNode: BiSeqNode[T] = new IndexedSeqNode(0)
  def lastNode: BiSeqNode[T] = new IndexedSeqNode(fastLength - 1)

  override def foreach[V](f: T => V): Unit = {
    FastLoop.ascending(0, length, 1) { i => f(apply(i)) }
  }

  // HELPER FUNCTIONS

  override def map[U](f: T => U): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def fastLength = self.length
    def fastApply(i: Int) = f(self(i))
  }

  def cartesianProduct[U](that: IndexedSeq[U]): IndexedSeq[(T, U)] = new AbstractIndexedSeq[(T, U)] {
    def fastLength = self.length * that.length
    def fastApply(i: Int) = (self(i / self.length), that(i % self.length))
  }

  /**
   * Returns the Cartesian product of two indexed sequences. The returning value is a table.
   */
  def cartesianProductToTable[U](that: IndexedSeq[U]): Table[(T, U)] = new AbstractTable[(T, U)] {
    def apply(i: Int, j: Int) = (self(i), that(j))
    def numRows = self.length
    def numCols = that.length
  }

  def concat[U >: T](that: IndexedSeq[U]): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def fastLength = self.length + that.length
    def fastApply(i: Int) = if (i < self.length) self(i) else that(i - self.length)
  }

  override def prepend[U >: T](x: U): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def fastLength = self.length + 1
    def fastApply(i: Int) = if (i == 0) x else self(i - 1)
  }

  override def append[U >: T](x: U): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def fastLength = self.length + 1
    def fastApply(i: Int) = if (i == self.length) x else self(i)
  }

  override def reduce[U >: T](f: (U, U) => U): U = {
    MapReduceOps.bySemigroup[U](length, x => self(x): U, f) // optimize through macros
  }

  /**
   * Rotates this sequence from the index specified.
   * @param j Rotation starts here
   * @return
   */
  override def rotate(j: Int): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    val len = self.length
    def fastApply(i: Int): T = self((j + i) % len)
    def fastLength: Int = len
  }

  override def slice(i: Int, j: Int): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastApply(n: Int) = self(i + n)
    def fastLength = j - i
  }

  /**
   * Pretends that this sequence is sorted under the given order.
   * (WARNING: Actual orderedness is not guaranteed! The user should make sure that it is sorted.)
   * @param U The implicit order
   * @return A sorted order
   */
  override def asIfSorted[U >: T](implicit U: WeakOrder[U]): SortedIndexedSeq[U] = new SortedIndexedSeq[U] {
    val orderOnValue: WeakOrder[U] = U
    def fastLength: Int = self.length
    def fastApply(i: Int): T = self.apply(i)
  }

  def interleave[U >: T](that: IndexedSeq[U]): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def fastLength = math.min(this.length, that.length) * 2
    def fastApply(i: Int) = if (i % 2 == 0) self(i / 2) else that(i / 2)
  }

  override def sliding(windowSize: Int, step: Int = 1): IndexedSeq[IndexedSeq[T]] = new AbstractIndexedSeq[IndexedSeq[T]] {
    def fastLength = (self.length - windowSize) / step + 1
    def fastApply(i: Int) = self.slice(step * i, step * i + windowSize)
  }

  // HELPER OBJECTS / CLASSES

  case class IndexedSeqNode(i: Int) extends BiSeqNode[T] {
    def isDummy = (i < 0) || (i >= length)
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
    def fastApply(i: Int): Nothing = throw new NoSuchElementException
    def fastLength: Int = 0
  }

  def fill[T](n: Int)(x: => T): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastLength = n
    def fastApply(i: Int) = x
  }

  def tabulate[T](n: Int)(f: Int => T): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastLength: Int = n
    def fastApply(i: Int): T = f(i)
  }

}

abstract class AbstractIndexedSeq[+T] extends IndexedSeq[T]