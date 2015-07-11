package poly.collection.node

import poly.collection._
import poly.collection.search._

/**
 * Basic trait for nodes. A node may contain a list of successor nodes.
 *
 * This serves as a common trait for sequence nodes, tree nodes and graph nodes.
 * Nodes provide a unified view for lists, trees and graphs.
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Node[+T] {
  def data: T
  def succ: Enumerable[Node[T]]
  override def toString = data.toString
}

object Node {
}

/**
 * Represents a node that has a list of predecessor nodes.
 * @since 0.1.0
 */
trait BackwardNode[+T] {
  def data: T
  def pred: Enumerable[BackwardNode[T]]
}

/**
 * Represents a node that has a list of successor nodes as well as a list of predecessor nodes.
 * @since 0.1.0
 */
trait BidiNode[+T] extends Node[T] with BackwardNode[T] {
  def data: T
  def succ: Enumerable[BidiNode[T]]
  def pred: Enumerable[BidiNode[T]]
}
