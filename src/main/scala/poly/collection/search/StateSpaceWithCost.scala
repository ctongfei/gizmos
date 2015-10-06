package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.util.specgroup._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StateSpaceWithCost[S, @sp(fdi) C] extends StateSpace[S] {

  implicit def groupOnCost: OrderedAdditiveGroup[C]
  def succWithCost(x: S): Traversable[(S, C)]
  def succ(x: S) = succWithCost(x).map(_._1)

  def uniformCostTreeTraversal(start: S): Iterable[S] =
    Iterable.ofIterator(new UniformCostTreeSearchIterator[S, C](start)(this))


}
