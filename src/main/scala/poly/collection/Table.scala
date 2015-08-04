package poly.collection

/**
 * A table is an indexed 2-D array.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Table[+T] extends ((Int, Int) => T) { self =>

  def numRows: Int

  def numCols: Int

  def size = numRows * numCols

  //TODO: make this lazy!
  def rows: Seq[Seq[T]] = Seq.tabulate(numRows)(i => Seq.tabulate(numCols)(j => self(i, j)))
  //TODO: make this lazy!
  def cols: Seq[Seq[T]] = Seq.tabulate(numCols)(j => Seq.tabulate(numRows)(i => self(i, j)))

  def map[U](f: T => U): Table[U] = new Table[U] {
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

  def grouped(i: Int, j: Int): Table[Table[T]] = ???

  def sliding(i: Int, j: Int): Table[Table[T]] = ???

  override def equals(that: Any) = that match {
    case other: Table[T] => ???
    case _ => false
  }

  override def toString() = ???

}

object Table {

}


trait DataMutableTable[T] extends Table[T] {
  def update(i: Int, j: Int, x: T): Unit
}

trait StructureMutableTable[T] extends DataMutableTable[T] {
  def clear(): Unit
  def appendRow(row: Seq[T]): Unit
  def appendCol(col: Seq[T]): Unit
  def removeRowAt(i: Int): Unit
  def removeColAt(j: Int): Unit
}
