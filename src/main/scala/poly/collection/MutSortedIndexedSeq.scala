package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait MutSortedIndexedSeq[T] extends SortedIndexedSeq[T] {

  def add(x: T): Unit

  def remove(x: T): Unit

  def deleteAt(i: Int): Unit

}
