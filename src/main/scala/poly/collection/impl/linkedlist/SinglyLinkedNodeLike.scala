package poly.collection.impl.linkedlist

/**
 * @author Tongfei Chen
 */
trait SinglyLinkedNodeLike[T, N <: SinglyLinkedNodeLike[T, N]] { self: N ⇒

  var data: T
  var next: N

}
