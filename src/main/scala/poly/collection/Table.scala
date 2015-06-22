package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Table[+T] extends ((Int, Int) => T) {

  def numRows: Int

  def numCols: Int

  def size = numRows * numCols

  def rows: Enumerable[Enumerable[T]]

  def cols: Enumerable[Enumerable[T]]

  def map[U](f: T => U): Table[U]

  override def equals(that: Any) = that match {
    case other: Table[T] => ???
    case _ => false
  }

  override def toString() = ???

}

