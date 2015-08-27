package poly.collection.search

import poly.algebra._
import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StateSpaceWithCost[S, C] extends StateSpace[S] {

  implicit def numericOfCost: OrderedRing[C]
  def succWithCost(x: S): Traversable[(S, C)]
  def succ(x: S) = succWithCost(x).map(_._1)

}
