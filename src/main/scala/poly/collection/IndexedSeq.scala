package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.collection.node._
import poly.macroutil._
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.language.implicitConversions

/**
 * Represents an indexed sequence.
 *
 * Indexed sequences should support efficient random access (typically O(1), sometimes may be O(log ''n'')).
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait IndexedSeq[+T] extends BiSeq[T] with HasKnownSize { self =>

  import IndexedSeq._

  def fastLength: Int

  def fastApply(i: Int): T

  @inline override def size = fastLength

  @inline override def length = fastLength

  @inline override def apply(i: Int) = fastApply(i)

  // Overridden newIterator method for performance.
  override def newIterator: Iterator[T] = new Iterator[T] {
    private[this] var i: Int = -1
    def current = self(i)
    def advance(): Boolean = {
      i += 1
      i < self.length
    }
  }

  def dummy: BiSeqNode[T] = new BiSeqNode[T] {
    def next = new NodeProxy[T](self, 0)
    def prev = new NodeProxy[T](self, length - 1)
    def data = throw new NoSuchElementException
    def isDummy = true
  }

  // Overridden foreach method for performance.
  override def foreach[V](f: T => V): Unit = {
    FastLoop.ascending(0, length, 1) { i => f(apply(i)) }
  }

  override def keys = Range(length)

  // HELPER FUNCTIONS

  override def map[U](f: T => U): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def fastLength = self.length
    def fastApply(i: Int) = f(self(i))
  }

  def cartesianProduct[U](that: IndexedSeq[U]): IndexedSeq[(T, U)] = new AbstractIndexedSeq[(T, U)] {
    val stride = that.length
    def fastLength = self.length * that.length
    def fastApply(i: Int) = (self(i / stride), that(i % stride))
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

  override def head = self(0)

  override def last = self(length - 1)

  override def tail = self.skip(1)

  override def init = self.take(length - 1)

  override def suffixes = Range(0, length) map skip

  override def prefixes = Range(length, 0, -1) map take

  override def take(n: Int) = slice(0, n)

  override def skip(n: Int) = slice(n, self.length)

  override def slice(i: Int, j: Int): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastApply(n: Int) = self(i + n)
    def fastLength = j - i
    override def slice(ii: Int, jj: Int) = { // optimize for nested slices
      require(i + jj <= j)
      self.slice(i + ii, i + jj)
    }
  }

  override def rotate(j: Int): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastApply(i: Int): T = self((j + i) % self.length)
    def fastLength: Int = self.length
    override def rotate(jj: Int) = self.rotate(j + jj)
  }

  override def reverse: IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastApply(i: Int) = self(self.length - 1 - i)
    def fastLength = self.length
    override def reverse = self
  }

  override def repeat(n: Int): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastApply(i: Int) = self(i % self.length)
    def fastLength = self.length * n
    override def repeat(nn: Int) = self.repeat(n * nn)
  }

  override def sliding(windowSize: Int, step: Int = 1): IndexedSeq[IndexedSeq[T]] = new AbstractIndexedSeq[IndexedSeq[T]] {
    def fastLength = (self.length - windowSize) / step + 1
    def fastApply(i: Int) = self.slice(step * i, step * i + windowSize)
  }

  def zip[U](that: IndexedSeq[U]): IndexedSeq[(T, U)] = new AbstractIndexedSeq[(T, U)] {
    def fastApply(i: Int) = (self(i), that(i))
    def fastLength = math.min(self.length, that.length)
  }

  def interleave[U >: T](that: IndexedSeq[U]): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def fastLength = math.min(self.length, that.length) * 2
    def fastApply(i: Int) = if (i % 2 == 0) self(i / 2) else that(i / 2)
  }

  /**
   * Pretends that this sequence is sorted under the given order.
   * (WARNING: Actual orderedness is not guaranteed! The user should make sure that it is sorted.)
 *
   * @param U The implicit order
   * @return A sorted order
   */
  override def asIfSorted[U >: T](implicit U: WeakOrder[U]): SortedIndexedSeq[U] = new SortedIndexedSeq[U] {
    val orderOnValue: WeakOrder[U] = U
    def fastLength: Int = self.length
    def fastApply(i: Int): T = self.apply(i)
  }

  def asIndexedSeq: IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastApply(i: Int) = self.fastApply(i)
    def fastLength = self.fastLength
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

  implicit def arrayAsIndexedSeq[T](a: Array[T]): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastLength = a.length
    def fastApply(i: Int) = a(i)
  }

  implicit def stringAsIndexedSeq(a: String): IndexedSeq[Char] = new AbstractIndexedSeq[Char] {
    def fastApply(i: Int) = a.charAt(i)
    def fastLength = a.length
  }

  implicit def Equiv[T: Equiv]: Equiv[IndexedSeq[T]] = new Equiv[IndexedSeq[T]] {
    def eq(x: IndexedSeq[T], y: IndexedSeq[T]) = {
      if (x.length != y.length) false
      else x.keys.forall(i => x(i) =~= y(i))
    }
  }

  class NodeProxy[T](val seq: IndexedSeq[T], val i: Int) extends BiSeqNode[T] {
    def isDummy = (i < 0) || (i >= seq.length)
    def data = seq(i)
    def next = if (i == seq.length - 1) BiSeqNode.dummy else new NodeProxy(seq, i + 1)
    def prev = if (i == 0) BiSeqNode.dummy else new NodeProxy(seq, i - 1)

    override def equals(that: Any) = that match {
      case that: NodeProxy[T] => (this.seq eq that.seq) && (this.i == that.i)
    }
  }


}

abstract class AbstractIndexedSeq[+T] extends IndexedSeq[T]