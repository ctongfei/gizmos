package poly.collection.node

import poly.collection._

/**
 * Represents a node that has a list of successor nodes as well as a list of predecessor nodes.
 * @since 0.1.0
 */
trait BiNode[+T] extends Node[T] with BackwardNode[T] { self =>
  def data: T
  def succ: Enumerable[BiNode[T]]
  def pred: Enumerable[BiNode[T]]

  override def reverse: BiNode[T] = new BiNode[T] {
    def pred = self.succ.map(_.reverse)
    def data = self.data
    def succ = self.pred.map(_.reverse)
    override def reverse = self
    override def isDummy = self.isDummy
  }

  override def isDummy = false
  override def notDummy = !isDummy
}
