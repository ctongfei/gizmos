package poly.collection

import poly.algebra._

/**
  * Represents a table, which is an indexed 2-D array.
  *
  * @author Tongfei Chen
  * @since 0.1.0
  */
trait Table[+T] extends Map[(Int, Int), T] with HasKnownSize { self =>

  def apply(i: Int, j: Int): T
  def numRows: Int
  def numCols: Int

  def apply(pair: (Int, Int)): T = apply(pair._1, pair._2)
  def ?(x: (Int, Int)): Option[T] = if (containsKey(x)) Some(self(x)) else None

  def equivOnKey = Equiv.default[(Int, Int)]

  def pairs: IndexedSeq[((Int, Int), T)] = new AbstractIndexedSeq[((Int, Int), T)] {
    def fastApply(idx: Int) = {
      val i = idx / numRows
      val j = idx / numCols
      ((i, j), self(i, j))
    }
    def fastLength = self.size
  }

  /**
   * Returns all the (row, col, elem) triples in this table.
   * @example {{{
   *    Table((1, 2), (3, 4)).triples
   *    == ((0,0,1), (0,1,2), (1,0,3), (1,1,4))
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
      def current = apply(i, j)
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

  override def map[U](f: T => U): Table[U] = new Table[U] {
    def numCols = self.numCols
    def numRows = self.numCols
    def apply(i: Int, j: Int) = f(self(i, j))
  }

  /** Transposes this table. */
  def transpose: Table[T] = new Table[T] {
    def numCols = self.numRows
    def numRows = self.numCols
    def apply(i: Int, j: Int) = self(j, i)
    override def transpose = self
  }

  def zip[U](that: Table[U]): Table[(T, U)] = new Table[(T, U)] {
    def numRows = math.min(self.numRows, that.numRows)
    def numCols = math.min(self.numRows, that.numRows)
    def apply(i: Int, j: Int) = (self(i, j), that(i, j))
  }

  def sliding(i: Int, j: Int, rowStep: Int = 1, colStep: Int = 1): Table[Table[T]] = ???

  override def equals(that: Any) = that match {
    case other: Table[T] => ???
    case _ => false
  }

  override def toString() = ???

}

object Table {

  def tabulate[T](nr: Int, nc: Int)(f: (Int, Int) => T): Table[T] = new AbstractTable[T] {
    def apply(i: Int, j: Int) = f(i, j)
    def numRows = nr
    def numCols = nc
  }

}

abstract class AbstractTable[T] extends Table[T]
