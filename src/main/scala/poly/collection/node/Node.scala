package poly.collection.node

/**
  * @author Tongfei Chen
  */
trait NodeLike[+T, +N <: NodeLike[T, N]] { self: N =>

  /** Returns the data on this node. */
  def data: T

  /** Tests if this node is a dummy node. */
  def isDummy: Boolean

  /** Tests if this node is not a dummy node. */
  @inline final def notDummy = !isDummy

  override def toString = if (notDummy) s"$data" else "<dummy>"
}


trait Node[+T] extends NodeLike[T, Node[T]]
