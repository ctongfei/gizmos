package poly.collection.immut

import poly.collection._
import poly.collection.node._

/**
 * Represents a lazy sequence in which traversed elements will be cached in a manner similar to a linked list.
 * This is similar to Scala's [[scala.collection.immutable.Stream]].
 * @author Tongfei Chen
 * @since 0.1.0
 */
sealed abstract class LazyList[+T] extends AbstractSeq[T] with SeqNode[T] { self =>

  import LazyList._

  final def headNode = self

  override def map[U](f: T => U): LazyList[U] = self match {
    case Empty      => Empty
    case Cons(h, t) => Cons(f(h), t map f)
  }

  def next: LazyList[T]
  final override def tail = next
  final override def head = data

  final override def drop(n: Int): LazyList[T] =
    if (n <= 0) self
    else self match {
      case Empty      => Empty
      case Cons(h, t) => t drop (n - 1)
    }

}

object LazyList {

  case object Empty extends LazyList[Nothing] {
    final def isDummy = true
    def next = Empty
    def data = throw new NoSuchElementException

    final override def toString = "()"
  }

  final class Cons[+T](h: T, t: => LazyList[T]) extends LazyList[T] with NonEmptySeq[T] {
    final def isDummy = false
    def next = t
    def data = h

    final override def toString = s"($h, ...)" // deliberately suppress evaluation of tail!
  }

  object Cons {
    def apply[T](h: T, t: => LazyList[T]) = new Cons[T](h, t)
    def unapply[T](c: Cons[T]) = Some(c.data, c.tail)
  }

  implicit class withConsOps[T](s: => LazyList[T]) {
    /** The cons operation on [[LazyList]]s. */
    def ::[U >: T](a: U) = LazyList.Cons(a, s)
  }

}
