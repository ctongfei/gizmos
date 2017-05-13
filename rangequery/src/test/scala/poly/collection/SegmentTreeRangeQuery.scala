package poly.collection

import algebra.instances.int._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
object SegmentTreeRangeQueryTest extends App {

  val a = ArraySeq(1, 2, 3, 4, 3, 2, 1)

  val rsq = SegmentTreeRangeQuery.from(a)

  val rmq = SegmentTreeRangeQuery.from(a)(new Monoid[Int] {
    def empty: Int = Int.MinValue
    def combine(x: Int, y: Int): Int = x max y
  })



  val bp = 0

}
