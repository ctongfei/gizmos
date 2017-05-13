package poly.collection.mut

import cats.implicits._
import poly.collection._
import poly.collection.factory._
import poly.collection.impl._
import poly.macroutil._

/**
 * @author Tongfei Chen
 */
class SegmentTreeRangeQuery[T] private(private val data: ResizableSeq[T])(implicit val monoid: Monoid[T])
  extends AbstractIndexedSeq[T] with ValueMutableIndexedSeq[T] with RangeMonoidQueryable[T]
{
  def fastLength = data.len / 2

  def fastApply(i: Int): T = data(length + i)

  def update(i: Int, x: T) = {
    var j = length + i
    data(j) = x
    while (j > 0) {
      var l = j
      var r = j
      if (j % 2 == 0) r = j + 1 // left child
      else l = j - 1 // right child
      data(j / 2) = data(l) |+| data(r)
      j /= 2
    }
  }

  def prefixAggregate(i: Int): T = rangeAggregate(0, i)

  def rangeAggregate(i: Int, j: Int) = {
    var l = length + i
    var r = length + j
    var s = monoid.empty
    while (l <= r) {
      if (l % 2 == 1) {
        s |+|= data(l)
        l += 1
      }
      if (r % 2 == 0) {
        s |+|= data(r)
        r -= 1
      }
      l /= 2
      r /= 2
    }
    s
  }
}

object SegmentTreeRangeQuery extends Factory1[Id, SegmentTreeRangeQuery, Monoid] {

  def newBuilder[T: Monoid]: Builder[T, SegmentTreeRangeQuery[T]] = new Builder[T, SegmentTreeRangeQuery[T]] {
    private[this] val array = new ResizableSeq[T]()
    override def sizeHint(n: Int) = array.ensureCapacity(n)
    def add(x: T): Unit = array.append_!(x)
    def result() = {
      val n = array.len
      val t = new ResizableSeq[T](2 * n)
      t.len = 2 * n
      FastLoop.ascending(0, n, 1) { i =>
        t(n + i) = array(i)
      }
      FastLoop.descending(n - 1, 0, -1) { i =>
        t(i) = t(2 * i) |+| t(2 * i + 1)
      }
      new SegmentTreeRangeQuery[T](t)
    }
  }
}
