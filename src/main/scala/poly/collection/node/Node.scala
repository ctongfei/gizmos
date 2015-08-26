package poly.collection.node

import poly.algebra.hkt._
import poly.collection._
import poly.collection.search._
import poly.collection.search.depr.StateSpace

/**
 * Basic trait for nodes. A node may contain a list of successor nodes.
 * This trait serves as a common trait for sequence nodes, tree nodes and graph nodes.
 * Nodes provide a unified view for lists, trees and graphs.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Node[+T] { self =>

  /** Returns the data on this node. */
  def data: T

  /** Returns a list of successors of this node. */
  def succ: Enumerable[Node[T]]

  def isDummy = false
  def notDummy = !isDummy

  override def toString = s"Node(${data.toString})"

  def map[U](f: T => U): Node[U] = new Node[U] {
    def data = f(self.data)
    def succ = self.succ.map(n => n.map(f))
  }
}

object Node {
  implicit def StateSpace[T]: StateSpace[Node[T]] = new StateSpace[Node[T]] {
    def succ(x: Node[T]) = x.succ
  }

  implicit object Functor extends Functor[Node] {
    def map[X, Y](nx: Node[X])(f: X => Y): Node[Y] = nx map f
  }
}
