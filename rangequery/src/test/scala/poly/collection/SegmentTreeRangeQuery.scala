package poly.collection

import poly.algebra._
import poly.algebra.syntax._
import poly.collection.mut._

/**
 * @author Tongfei Chen
 */
object SegmentTreeRangeQueryTest extends App {

  val a = ArraySeq(1, 2, 3, 4, 3, 2, 1)

  val rsq = SegmentTreeRangeQuery.from(a)(Ring[Int].asMonoidWithAdd)

  val rmq = SegmentTreeRangeQuery.from(a)(Monoid.create(Math.min, Int.MaxValue))



  val bp = 0

}
