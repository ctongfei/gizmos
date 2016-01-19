package poly.collection

import poly.collection.mut._

private[collection]
class ArrayAsIndexedSeq[T](val underlying: Array[T]) extends DataMutableIndexedSeq[T] {
  def fastLength = underlying.length
  def fastApply(i: Int) = underlying(i)
  def update(i: Int, x: T) = underlying(i) = x
}

private[collection]
class StringAsIndexedSeq(val underlying: String) extends IndexedSeq[Char] {
  def fastLength = underlying.length
  def fastApply(i: Int) = underlying.charAt(i)
}
