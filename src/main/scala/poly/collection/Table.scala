package poly.collection

import poly.algebra._

/**
 * A table is an indexed 2-D array.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Table[+T] extends Map[(Int, Int), T] { self =>

  def apply(i: Int, j: Int): T
  def numRows: Int
  def numCols: Int

  def apply(pair: (Int, Int)): T = apply(pair._1, pair._2)
  def ?(x: (Int, Int)): Option[T] = if (containsKey(x)) Some(self(x)) else None

  def equivOnKey = Equiv.create((x, y) => x._1 == y._1 && x._2 == y._2)

  def pairs = for { (i: Int) ← Range(numRows); j ← Range(numCols) } yield (i → j) → self(i, j)

  def newIterator: Iterator[T] = new Iterator[T] {
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

}

abstract class AbstractTable[T] extends Table[T]






