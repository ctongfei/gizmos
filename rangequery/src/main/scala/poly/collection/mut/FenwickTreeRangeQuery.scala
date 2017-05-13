package poly.collection.mut

import cats.implicits._
import poly.collection._
import poly.collection.factory._
import poly.collection.impl._

/**
 * A Fenwick tree.
 * Fenwick trees provides efficient methods for the computation and manipulation
 * of the prefix aggregate of a sequence of values given a group endowed upon the data type.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class FenwickTreeRangeQuery[T] private(private val data: ResizableSeq[T])(implicit val group: Group[T])
  extends AbstractIndexedSeq[T] with ValueMutableIndexedSeq[T] with RangeGroupQueryable[T] {

  import FenwickTreeRangeQuery._

  /** $O1 @inheritdoc */
  def fastLength = data.length - 1

  /** $Ologn @inheritdoc */
  def fastApply(idx: Int) = {
    var i = idx + 1
    var s = data(i)
    val z = i - lowBit(i)
    i -= 1
    while (i != z) {
      s |-|= data(i)
      i -= lowBit(i)
    }
    s
  }

  /** $Ologn Returns the sum of the first ''n'' elements of this list.  */
  def prefixAggregate(n: Int) = {
    var i = n
    var s = group.empty
    while (i != 0) {
      s |+|= data(i)
      i -= lowBit(i)
    }
    s
  }

  /** $Ologn Returns the sum of the elements in the slice [''i'', ''j'').  */
  def rangeAggregate(i: Int, j: Int) = group.remove(prefixAggregate(j), prefixAggregate(i))

  /** $Ologn Increments the ''i''-th element by ''δ''. */
  def increment(i: Int, δ: T) = {
    var j = i + 1
    while (j < data.length) {
      data(j) |+|= δ
      j += lowBit(j)
    }
  }

  def update(idx: Int, value: T) = increment(idx, value |-| this(idx))

  def sum = prefixAggregate(length)

}

object FenwickTreeRangeQuery extends Factory1[Id, FenwickTreeRangeQuery, Group] {

  @inline private[poly] def lowBit(x: Int) = x & -x

  implicit def newBuilder[T](implicit T: Group[T]): Builder[T, FenwickTreeRangeQuery[T]] = new Builder[T, FenwickTreeRangeQuery[T]] {
    private[this] val data = ResizableSeq[T](T.empty)
    def add(x: T) = {
      var i = data.len
      var s = x
      val z = i - lowBit(i)
      i -= 1
      while (i != z) {
        s |+|= data(i)
        i -= lowBit(i)
      }
      data.append_!(s)
    }
    override def sizeHint(n: Int) = data.ensureCapacity(n)
    def result = new FenwickTreeRangeQuery(data)
  }
}
