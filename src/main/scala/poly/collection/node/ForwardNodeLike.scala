package poly.collection.node

import poly.algebra._
import poly.collection._
import poly.collection.search._

/**
 * Basic trait for forward nodes. A forward node may contain a list of successor nodes.
 *
 * This trait serves as a common trait for sequence nodes, tree nodes and graph nodes.
 * Nodes provide a unified view for lists, trees and graphs, as well as search nodes
 * in searching algorithms provided in package [[poly.collection.search]].
 *
 * @author Tongfei Chen
 * @since 0.1.0
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
    def equivOnKey = Equiv.default[N]
    def succ(x: N) = x.succ
  }
}

trait ForwardNode[+T] extends Node[T] with ForwardNodeLike[T, ForwardNode[T]] { self =>

  def reverse: BackwardNode[T] = new BackwardNode[T] {
    def pred = self.succ.map(_.reverse)
    def data = self.data
    override def reverse = self
    def isDummy = self.isDummy
  }

  def map[U](f: T => U): ForwardNode[U] = new ForwardNode[U] {
    def data = f(self.data)
    def succ = self.succ.map(_.map(f))
    def isDummy = self.isDummy
  }

  def zip[U](that: ForwardNode[U]): ForwardNode[(T, U)] = new ForwardNode[(T, U)] {
    def data = (self.data, that.data)
    def succ = (self.succ zip that.succ).map { case (a, b) => a zip b }
    def isDummy = self.isDummy || that.isDummy
  }

}