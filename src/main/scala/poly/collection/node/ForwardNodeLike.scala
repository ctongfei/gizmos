package poly.collection.node

import poly.algebra._
import poly.collection._
import poly.collection.search._

/**
 * Contains searching algorithms for all forward nodes.
 * @author Tongfei Chen
 */
trait ForwardNodeLike[+T, +N <: ForwardNodeLike[T, N]] extends NodeLike[T, N] { self: N =>

  import ForwardNodeLike._

  /** Returns a list of successors of this node. */
  def succ: Iterable[N]

  def depthFirstTreeTraversal = StateSpace[T, N].depthFirstTreeTraversal(self)
  def breadthFirstTreeTraversal = StateSpace[T, N].breadthFirstTreeTraversal(self)
  def depthFirstTraversal = StateSpace[T, N].depthFirstTraversal(self)
  def breadthFirstTraversal = StateSpace[T, N].breadthFirstTraversal(self)

  def depthFirstSearch(goal: T => Boolean) = StateSpace[T, N].depthFirstSearch(self, x => goal(x.data))
  def breadthFirstSearch(goal: T => Boolean) = StateSpace[T, N].breadthFirstSearch(self, x => goal(x.data))
}

object ForwardNodeLike {
  implicit def StateSpace[T, N <: ForwardNodeLike[T, N]]: StateSpace[N] = new StateSpace[N] {
    def equivOnState = Equiv.default[N]
    def succ(x: N) = x.succ
  }
}
