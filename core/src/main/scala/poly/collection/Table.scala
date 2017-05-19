package poly.collection

import cats.implicits._
import poly.collection.node._
import poly.macroutil._
import poly.collection.typeclass._

/**
 * Represents a table, which is a rectangular (not jagged) indexed 2-D array.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Table[+T] extends PartialFunction[(Int, Int), T] { self =>

  /** Returns the element at the ''i''-th row and ''j''-th column in this table. */
  def apply(i: Int, j: Int): T

  /** Returns the number of rows in this table. */
  def numRows: Int

  /** Returns the number of columns in this table. */
  def numCols: Int

  def apply(pair: (Int, Int)): T = apply(pair._1, pair._2)

  def isDefinedAt(x: (Int, Int)) = {
    val (i, j) = x
    0 <= i && i < numRows && 0 <= j && j < numCols
  }

  def indexSet: Set[(Int, Int)] = new AbstractSet[(Int, Int)] {
    def keys = Range(numRows) product Range(numCols)
    def contains(x: (Int, Int)) = {
      val (i, j) = x
      i >= 0 && i < numRows && j >= 0 && j < numCols
    }
    implicit def keyEq = Hash.default[(Int, Int)]
  }


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
      val j = idx % numCols
      (i, j, self(i, j))
    }
    def fastLength = self.size
  }

  def elements = triples map third

  def size = numRows * numCols

  def topLeftNode = new Table.NodeProxy(self, 0, 0)

  def bottomRightNode = new Table.NodeProxy(self, numRows - 1, numCols - 1)

  def row(i: Int): IndexedSeq[T] = Range(numCols) map (j => self(i, j))

  def col(j: Int): IndexedSeq[T] = Range(numRows) map (i => self(i, j))

  def rows: IndexedSeq[IndexedSeq[T]] = Range(numRows) map row

  def cols: IndexedSeq[IndexedSeq[T]] = Range(numCols) map col

  def curry = rows

  def slice(topLeftI: Int, topLeftJ: Int, botRightI: Int, botRightJ: Int): Table[T] =
    new TableT.Sliced(self, topLeftI, topLeftJ, botRightI, botRightJ)

  def takeRows(i: Int) = slice(0, 0, i, self.numCols)

  def dropRows(i: Int) = slice(i, 0, self.numRows, self.numCols)

  def takeCols(j: Int) = slice(0, 0, self.numRows, j)

  def dropCols(j: Int) = slice(0, j, self.numRows, self.numCols)

  def map[U](f: T => U): Table[U] = new TableT.Mapped(self, f)

  /** Transposes this table. */
  def transpose: Table[T] = new TableT.Transposed(self)

  def zip[U](that: Table[U]) = zipWith(that) { (t, u) => (t, u) }

  def zipWith[U, V](that: Table[U])(f: (T, U) => V): Table[V] = new TableT.ZippedWith(self, that, f)

  def sliding(i: Int, j: Int, rowStep: Int = 1, colStep: Int = 1): Table[Table[T]] = new TableT.Sliding(self, i, j, rowStep, colStep)


  def asMap: Map[(Int, Int), T] = new TableT.AsMap(self)

  // OVERRIDING JAVA METHODS
  override def equals(that: Any) = that match {
    case that: Table[T] => Table.Eq[T](Hash.default[T]).eqv(self, that)
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

  class NodeProxy[+T](val table: Table[T], val i: Int, val j: Int) extends BidiNode[T] {
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

  def Eq[T](implicit T: Eq[T]): Eq[Table[T]] = new TableT.TableEq()(T)

}

abstract class AbstractTable[T] extends Table[T]

private[poly] object TableT {

  class AsMap[T](self: Table[T]) extends AbstractMap[(Int, Int), T] {
    def keySet: Set[(Int, Int)] = self.indexSet
    def ?(k: (Int, Int)): Option[T] = if (containsKey(k)) Some(self(k)) else None
    def apply(k: (Int, Int)): T = self(k)
    override def pairs = self.triples map { case (i, j, e) => ((i, j), e) }
  }

  class TableEq[T](implicit T: Eq[T]) extends Eq[Table[T]] {
    def eqv(x: Table[T], y: Table[T]): Boolean = {
      if (x.numRows != y.numRows) return false
      if (x.numCols != y.numCols) return false
      FastLoop.ascending(0, x.numRows, 1) { i =>
        FastLoop.ascending(0, x.numCols, 1) { j =>
          if (x(i, j) =!= y(i, j)) return false
        }
      }
      true
    }
  }

  class Mapped[T, U](self: Table[T], f: T => U) extends AbstractTable[U] {
    def numCols = self.numCols
    def numRows = self.numRows
    def apply(i: Int, j: Int) = f(self(i, j))
  }

  class ZippedWith[T, U, V](self: Table[T], that: Table[U], f: (T, U) => V) extends AbstractTable[V] {
    val numRows = math.min(self.numRows, that.numRows)
    val numCols = math.min(self.numRows, that.numRows)
    def apply(i: Int, j: Int) = f(self(i, j), that(i, j))
  }

  class Transposed[T](self: Table[T]) extends AbstractTable[T] {
    val numCols = self.numRows
    val numRows = self.numCols
    def apply(i: Int, j: Int) = self(j, i)
    override def transpose = self
  }

  class Sliced[T](self: Table[T], topLeftI: Int, topLeftJ: Int, botRightI: Int, botRightJ: Int) extends AbstractTable[T] {
    val numRows = botRightI - topLeftI
    val numCols = botRightJ - topLeftJ
    def apply(i: Int, j: Int) = self.apply(topLeftI + i, topLeftJ + j)
  }

  class Sliding[T](self: Table[T], i: Int, j: Int, rowStep: Int = 1, colStep: Int = 1) extends AbstractTable[Table[T]] {
    val numRows = (self.numRows - i) / rowStep
    val numCols = (self.numCols - j) / colStep
    def apply(m: Int, n: Int) = self.slice(m * rowStep, n * colStep, m * rowStep + i, n * colStep + j)
  }
}