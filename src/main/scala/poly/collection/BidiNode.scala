package poly.collection

/**
 * Basic trait for bi-directional nodes.
 *
 * Serves as a common trait for list nodes, tree nodes and graph nodes.
 * Nodes provide a unified view for lists, trees and graphs.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait BidiNode[+T] extends ForwardNode[T] with BackwardNode[T] {

  def data: T
  def descendants: Enumerable[BidiNode[T]]
  def ancestors: Enumerable[BidiNode[T]]

  override def toString = s"Node($data)"

}


trait ForwardNode[+T] {
  def data: T
  def descendants: Enumerable[ForwardNode[T]]
}

trait BackwardNode[+T] {
  def data: T
  def ancestors: Enumerable[BackwardNode[T]]
}

