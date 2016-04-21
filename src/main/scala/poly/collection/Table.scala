package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.macroutil._

/**
 * Represents a table, which is a rectangular (not jagged) indexed 2-D array.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Table[+T] extends Map[(Int, Int), T] with HasKnownSize { self =>

  /** Returns the element at the ''i''-th row and ''j''-th column in this table. */
  def apply(i: Int, j: Int): T

  /** Returns the number of rows in this table. */
  def numRows: Int

  /** Returns the number of columns in this table. */
  def numCols: Int

  def apply(pair: (Int, Int)): T = apply(pair._1, pair._2)
  def ?(x: (Int, Int)): Option[T] = if (containsKey(x)) Some(self(x)) else None

  def equivOnKeys = Equiv.default[(Int, Int)]

  def pairs = triples map { case (i, j, e) => ((i, j), e) }

  /**
   * Returns all the (row, col, elem) triples in this table.
   * @example {{{
   *    ┌      ┐
   *    │ 1  2 │
   *    │ 3  4 │.triples == ((0,0,1), (0,1,2), (1,0,3), (1,1,4))
   *    └      ┘
   * }}}
   */
  def triples: IndexedSeq[(Int, Int, T)] = new AbstractIndexedSeq[(Int, Int, T)] {
    def fastApply(idx: Int) = {
      val i = idx / numRows
      val j = idx / numCols
      (i, j, self(i, j))
    }
    def fastLength = self.size
  }

  def elements = Iterable.ofIterator {
    new Iterator[T] {
      private[this] var i = 0
      private[this] var j = 0
      def advance() = {
        if (i < numRows) {
          if (j < numCols)
            j += 1
          else {
            i += 1
            j = 0
          }
          true
        }
        else false
      }
      def current = self.apply(i, j)
    }
  }

  override def size = numRows * numCols

  def containsKey(x: (Int, Int)): Boolean = {
    val (i, j) = x
    i >= 0 && i < numRows && j >= 0 && j < numCols
  }

  def row(i: Int): IndexedSeq[T] = IndexedSeq.tabulate(numCols)(j => self(i, j))

  def col(j: Int): IndexedSeq[T] = IndexedSeq.tabulate(numRows)(i => self(i, j))

  def rows: IndexedSeq[IndexedSeq[T]] = IndexedSeq.tabulate(numRows)(row)

  def cols: IndexedSeq[IndexedSeq[T]] = IndexedSeq.tabulate(numCols)(col)

  override def map[U](f: T => U): Table[U] = new AbstractTable[U] {
    def numCols = self.numCols
    def numRows = self.numCols
    def apply(i: Int, j: Int) = f(self(i, j))
  }

  /** Transposes this table. */
  def transpose: Table[T] = new AbstractTable[T] {
    def numCols = self.numRows
    def numRows = self.numCols
    def apply(i: Int, j: Int) = self(j, i)
    override def transpose = self
  }

  def zip[U](that: Table[U]): Table[(T, U)] = new AbstractTable[(T, U)] {
    def numRows = math.min(self.numRows, that.numRows)
    def numCols = math.min(self.numRows, that.numRows)
    def apply(i: Int, j: Int) = (self(i, j), that(i, j))
  }

  def sliding(i: Int, j: Int, rowStep: Int = 1, colStep: Int = 1): Table[Table[T]] = ???

  override def equals(that: Any) = that match {
    case other: Table[T] => Table.Equiv[T](Equiv.default[T]).eq(self, other)
    case _ => false
  }

  override def toString() = ???

}

object Table {

  def fill[T](nr: Int, nc: Int)(x: => T): Table[T] = new AbstractTable[T] {
    def numRows: Int = nr
    def numCols: Int = nc
    def apply(i: Int, j: Int): T = x

  }

  def tabulate[T](nr: Int, nc: Int)(f: (Int, Int) => T): Table[T] = new AbstractTable[T] {
    def apply(i: Int, j: Int) = f(i, j)
    def numRows = nr
    def numCols = nc
  }

  implicit def Equiv[T: Equiv]: Equiv[Table[T]] = new Equiv[Table[T]] {
    def eq(x: Table[T], y: Table[T]): Boolean = {
      if (x.numRows != y.numRows) return false
      if (x.numCols != y.numCols) return false
      FastLoop.ascending(0, x.numRows, 1) { i =>
        FastLoop.descending(0, x.numCols, 1) { j =>
          if (x(i, j) !== y(i, j)) return false
        }
      }
      true
    }
  }

}

abstract class AbstractTable[T] extends Table[T]
