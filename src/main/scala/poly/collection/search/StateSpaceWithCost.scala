package poly.collection.search

import poly.algebra._
import poly.collection._
import poly.util.specgroup._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait StateSpaceWithCost[S, @sp(fdi) C] extends StateSpace[S] {

  implicit def monoidOfCost: AdditiveMonoid[C]
  def succWithCost(x: S): Traversable[(S, C)]
  def succ(x: S) = succWithCost(x).map(_._1)

  def uniformCostTreeTraversal(start: S)(implicit ev1: WeakOrder[C], ev2: AdditiveMonoid[C]): Iterable[S] =
    Iterable.ofIterator(new UniformCostTreeSearchIterator[S, C](start)(ev1, ev2, this))


}
