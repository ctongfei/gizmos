package poly.collection

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait MutableIndexedSortedSeq[T] extends IndexedSortedSeq[T] {

  def add(x: T): Unit

  def remove(x: T): Unit

  def deleteAt(i: Int): Unit

}
