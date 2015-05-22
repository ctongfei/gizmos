package poly.collection

/**
 * Serves as a common trait for list nodes, tree nodes and graph nodes.
 * Nodes provide a unified view for lists, trees and graphs.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Node[+T] extends ForwardNode[T] with BackwardNode[T] {

  def data: T
  def descendants: Enumerable[Node[T]]
  def ancestors: Enumerable[Node[T]]

}


trait ForwardNode[+T] {
  def data: T
  def descendants: Enumerable[ForwardNode[T]]
}

trait BackwardNode[+T] {
  def data: T
  def ancestors: Enumerable[BackwardNode[T]]
}

