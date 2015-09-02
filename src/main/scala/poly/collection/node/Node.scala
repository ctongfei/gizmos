package poly.collection.node

import poly.algebra.hkt._
import poly.collection._
import poly.collection.search._

/**
 * Basic trait for nodes. A node may contain a list of successor nodes.
 *
 * This trait serves as a common trait for sequence nodes, tree nodes and graph nodes.
 * Nodes provide a unified view for lists, trees and graphs.
 *
 * Explicitly inherits the deprecated trait [[scala.NotNull]] to emphasize that a node
 * in Poly-collection should never be null.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Node[+T] extends NotNull { self =>

  /** Returns the data on this node. */
  def data: T

  /** Returns a list of successors of this node. */
  def succ: Iterable[Node[T]]

  def isDummy = false
  def notDummy = !isDummy

  override def toString = s"Node($data)"

  def reverse: BackwardNode[T] = new BackwardNode[T] {
    def pred = self.succ.map(_.reverse)
    def data = self.data
    override def reverse = self
    override def isDummy = self.isDummy
  }

  def map[U](f: T => U): Node[U] = new Node[U] {
    def data = f(self.data)
    def succ = self.succ.map(n => n.map(f))
    override def isDummy = self.isDummy
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
