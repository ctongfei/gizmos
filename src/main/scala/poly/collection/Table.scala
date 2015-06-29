package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Table[+T] extends ((Int, Int) => T) {

  def numRows: Int

  def numCols: Int

  def size = numRows * numCols

  def rows: Seq[Seq[T]]

  def cols: Seq[Seq[T]]

  def map[U](f: T => U): Table[U]

  def zip[U](that: Table[U]): Table[(T, U)]

  def grouped(i: Int, j: Int): Table[Table[T]]

  def sliding(i: Int, j: Int): Table[Table[T]]

  override def equals(that: Any) = that match {
    case other: Table[T] => ???
    case _ => false
  }

  override def toString() = ???

}

object Table {

}
