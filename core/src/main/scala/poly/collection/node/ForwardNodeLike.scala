package poly.collection.node

import cats.implicits._
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

  def depthFirstSearch(goal: T => Boolean): BidiSeq[N] = StateSpace[T, N].depthFirstSearch(self)(x => goal(x.data))
  def breadthFirstSearch(goal: T => Boolean): BidiSeq[N] = StateSpace[T, N].breadthFirstSearch(self)(x => goal(x.data))
  def depthFirstSearch[U >: T : Eq](goal: U): BidiSeq[N] = depthFirstSearch(goal === _)
  def breadthFirstSearch[U >: T : Eq](goal: U): BidiSeq[N] = breadthFirstSearch(goal === _)

  override def toString = s"[$dataString → ${succ map {_.data}}]"
}

object ForwardNodeLike {
  implicit def StateSpace[T, N <: ForwardNodeLike[T, N]]: EqStateSpace[N] = new AbstractEqStateSpace[N] {
    def keyEq = Hashing.default[N]
    def succ(x: N) = x.succ
  }
}

trait ForwardNode[+T] extends Node[T] with ForwardNodeLike[T, ForwardNode[T]]
