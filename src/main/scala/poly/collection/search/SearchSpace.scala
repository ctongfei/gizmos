package poly.collection.search

import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait SearchSpace[X, D] {

  def succ: Enumerable[X]
  def succWithWeight: Enumerable[(X, D)]

}

object SearchSpace {

}
