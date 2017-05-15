package poly.collection.impl

import poly.collection._
import poly.collection.specgroup._

/**
 * @author Tongfei Chen
 */
trait IntervalTreeNodeLike[@sp(di) R, +N >: Null <: IntervalTreeNodeLike[R, N]] { self: N =>

  def interval: Interval[R]


}
