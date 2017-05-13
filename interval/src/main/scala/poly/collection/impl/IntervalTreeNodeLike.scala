package poly.collection.impl

import poly.algebra.specgroup._
import poly.collection.Interval

/**
 * @author Tongfei Chen
 */
trait IntervalTreeNodeLike[@sp(di) R, +N >: Null <: IntervalTreeNodeLike[R, N]] { self: N =>

  def interval: Interval[R]


}
