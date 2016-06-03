package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.node._
import poly.macroutil._

/**
 * Represents a table, which is a rectangular (not jagged) indexed 2-D array.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Table[+T] extends Map[(Int, Int), T] { self =>

  /** Returns the element at the ''i''-th row and ''j''-th column in this table. */
  def apply(i: Int, j: Int): T

  /** Returns the number of rows in this table. */
  def numRows: Int

  /** Returns the number of columns in this table. */
  def numCols: Int

  def apply(pair: (Int, Int)): T = apply(pair._1, pair._2)
  def ?(x: (Int, Int)): Option[T] = if (containsKey(x)) Some(self(x)) else None

  def eqOnKeys = Eq.default[(Int, Int)]

  def pairs = triples map { case (i, j, e) => ((i, j), e) }

  /**
   * Returns all the (row, col, elem) triples in this table.
   *
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
      val j = idx % numCols
      (i, j, self(i, j))
    }
    def fastLength = self.size
  }

  def elements = triples map third

  override def size = numRows * numCols

  def topLeftNode = new Table.NodeProxy(self, 0, 0)
  def bottomRightNode = new Table.NodeProxy(self, numRows - 1, numCols - 1)

  def containsKey(x: (Int, Int)): Boolean = {
    val (i, j) = x
    i >= 0 && i < numRows && j >= 0 && j < numCols
  }

  def row(i: Int): IndexedSeq[T] = Range(numCols) map (j => self(i, j))

  def col(j: Int): IndexedSeq[T] = Range(numRows) map (i => self(i, j))

  def rows: IndexedSeq[IndexedSeq[T]] = Range(numRows) map row

  def cols: IndexedSeq[IndexedSeq[T]] = Range(numCols) map col

  def curry = rows

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
    def numRows = min(self.numRows, that.numRows)
    def numCols = min(self.numRows, that.numRows)
    def apply(i: Int, j: Int) = (self(i, j), that(i, j))
  }

  def sliding(i: Int, j: Int, rowStep: Int = 1, colStep: Int = 1): Table[Table[T]] = ???

  // OVERRIDING JAVA METHODS

  override def equals(that: Any) = that match {
    case that: Table[T] => Table.Eq[T](Eq.default[T]).eq(self, that)
    case _ => false
  }

  override def toString = {
    val s = self map { _.toString }
    val l = s.elements.map(_.length).max
    val r0 =                          "┌ " + " " * (numCols * (l + 2) - 2)             + " ┐"
    val lines = s.rows.map { xs =>    "│ " + xs.map(_.padTo(l, ' ')).buildString("  ") + " │" }
    val rn =                          "└ " + " " * (numCols * (l + 2) - 2)             + " ┘"
    "\n" + r0 + "\n" + lines.buildString("\n") + "\n" + rn + "\n"
  }

}

object Table {

  class NodeProxy[+T](val table: Table[T], val i: Int, val j: Int) extends BiNode[T] {
    def right = new NodeProxy(table, i, j + 1)
    def left = new NodeProxy(table, i, j - 1)
    def up = new NodeProxy(table, i - 1, j)
    def down = new NodeProxy(table, i + 1, j)
    def succ = Seq(right, down)
    def pred = Seq(left, up)
    def data = table(i, j)
    def isDummy = (i < 0) || (i >= table.numRows) || (j < 0) || (j >= table.numCols)
  }

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

  def Eq[T: Eq]: Eq[Table[T]] = new Eq[Table[T]] {
    def eq(x: Table[T], y: Table[T]): Boolean = {
      if (x.numRows != y.numRows) return false
      if (x.numCols != y.numCols) return false
      FastLoop.ascending(0, x.numRows, 1) { i =>
        FastLoop.ascending(0, x.numCols, 1) { j =>
          if (x(i, j) !== y(i, j)) return false
        }
      }
      true
    }
  }

}

abstract class AbstractTable[T] extends Table[T]
