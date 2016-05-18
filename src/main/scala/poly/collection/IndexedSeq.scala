package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.node._
import poly.macroutil._
import scala.annotation.unchecked.{uncheckedVariance => uv}
import scala.language.implicitConversions

/**
 * Represents an indexed sequence.
 * Indexed sequences should support efficient random access (typically O(1), sometimes may be O(log ''n'')).
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait IndexedSeq[+T] extends BiSeq[T] { self =>

  import IndexedSeq._

  def fastLength: Int

  def fastApply(i: Int): T

  @inline final override def size = fastLength

  @inline final override def length = fastLength

  /** Returns the ''i''-th element of this sequence. $O1 (sometimes O(log ''n''): e.g. [[poly.collection.mut.FenwickTree]]). */
  @inline final override def apply(i: Int) = fastApply(i)

  // Overridden newIterator method for performance.
  override def newIterator: Iterator[T] = new Iterator[T] {
    private[this] var i: Int = -1
    def current = self(i)
    def advance(): Boolean = {
      i += 1
      i < self.length
    }
  }

  // Overridden foreach method for performance.
  override def foreach[V](f: T => V): Unit = {
    FastLoop.ascending(0, length, 1) { i => f(apply(i)) }
  }


  def headNode = new NodeProxy[T](self, 0)
  def lastNode = new NodeProxy[T](self, length - 1)

  override def sizeKnown = true

  override def pairs: SortedIndexedSeq[(Int, T @uv)] =
    IndexedSeq.tabulate(length)(i => i → self(i)).asIfSorted(Order by firstOfPair)

  override def keys = Range(length)

  // HELPER FUNCTIONS

  override def map[U](f: T => U): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def fastLength = self.length
    def fastApply(i: Int) = f(self(i))
  }

  def product[U](that: IndexedSeq[U]): IndexedSeq[(T, U)] = new AbstractIndexedSeq[(T, U)] {
    private[this] val stride = that.length
    def fastLength = self.length * that.length
    def fastApply(i: Int) = (self(i / stride), that(i % stride))
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

  override def fold[U >: T](z: U)(f: (U, U) => U): U = {
    MapReduceOps.byMonoid[U](length, x => self(x): U, z, f) // optimize through macros
  }

  override def head = self(0)

  override def last = self(length - 1)

  override def tail = self.skip(1)

  override def init = self.take(length - 1)

  override def suffixes = (0 ~~< length) map skip

  override def prefixes = (length ~~> 0) map take

  override def take(n: Int) = slice(0, n)

  override def skip(n: Int) = slice(n, self.length)

  override def slice(i: Int, j: Int): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastApply(n: Int) = self(i + n)
    def fastLength = j - i
    override def slice(ii: Int, jj: Int) = { // optimize for nested slices
      self.slice(i + ii, math.min(i + jj, j))
    }
  }

  override def rotate(j: Int): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastApply(i: Int): T = self((j + i) % self.length)
    def fastLength: Int = self.length
    override def rotate(jj: Int) = self.rotate(j + jj) // optimize for nested rotates
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

  def zipWith[U, V](that: IndexedSeq[U])(f: (T, U) => V): IndexedSeq[V] = new AbstractIndexedSeq[V] {
    def fastApply(i: Int) = f(self(i), that(i))
    def fastLength = math.min(self.length, that.length)
  }

  def interleave[U >: T](that: IndexedSeq[U]): IndexedSeq[U] = new AbstractIndexedSeq[U] {
    def fastLength = math.min(self.length, that.length) * 2
    def fastApply(i: Int) = if (i % 2 == 0) self(i / 2) else that(i / 2)
  }

  /**
    * Rearranges the elements in this indexed sequence according to a permutation. $LAZY
   *
   * @param p A permutation which is of the same length as this sequence
    * @return A permuted sequence
    * @example {{{('a', 'b', 'c') permuteBy Permutation(1, 2, 0) == ('b', 'c', 'a')}}}
    */
  def permuteBy(p: Permutation): IndexedSeq[T] = new AbstractIndexedSeq[T] {
    def fastApply(i: Int) = self(p.invert(i))
    def fastLength = self.fastLength
    override def permuteBy(q: Permutation) = self.permuteBy(q compose p)
  }

  override def asIfSorted(implicit T: Order[T]): SortedIndexedSeq[T @uv] = new SortedIndexedSeq[T] {
    def orderOnElements = T
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

  //TODO: should be implicit, but results in ambiguous implicits because of contravariant typeclass
  def Eq[T: Eq]: Eq[IndexedSeq[T]] = new Eq[IndexedSeq[T]] {
    def eq(x: IndexedSeq[T], y: IndexedSeq[T]): Boolean = {
      if (x.fastLength != y.fastLength) false
      else {
        FastLoop.ascending(0, x.fastLength, 1) { i =>
          if (x(i) !== y(i)) return false
        }
        true
      }
    }
  }

  class NodeProxy[+T](val seq: IndexedSeq[T], val i: Int) extends BiSeqNode[T] {
    def isDummy = (i < 0) || (i >= seq.length)
    def data = seq(i)
    def next = if (i >= seq.length - 1) BiSeqNode.dummy else new NodeProxy(seq, i + 1)
    def prev = if (i <= 0) BiSeqNode.dummy else new NodeProxy(seq, i - 1)

    override def equals(that: Any) = that match {
      case that: NodeProxy[T] => (this.seq eq that.seq) && (this.i == that.i)
      case _ => false
    }
  }
}


abstract class AbstractIndexedSeq[+T] extends IndexedSeq[T]