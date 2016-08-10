package poly.collection.immut

import poly.collection._
import poly.collection.node._

/**
 * Represents a lazy stream in which traversed elements will be cached in a manner similar to a linked list.
 * @author Tongfei Chen
 * @since 0.1.0
 */
sealed abstract class LazyFSeq[+T] extends AbstractSeq[T] with SeqNode[T] { self =>

  import LazyFSeq._

  final def headNode = self

  override def map[U](f: T => U): LazyFSeq[U] = self match {
    case Empty      => Empty
    case Cons(h, t) => Cons(f(h), t map f)
  }

  def next: LazyFSeq[T]
  final override def tail = next
  final override def head = data

  final override def drop(n: Int): LazyFSeq[T] =
    if (n <= 0) self
    else self match {
      case Empty      => Empty
      case Cons(h, t) => t drop (n - 1)
    }

}

object LazyFSeq {

  case object Empty extends LazyFSeq[Nothing] {
    final def isDummy = true
    def next = Empty
    def data = throw new NoSuchElementException

    final override def toString = "()"
  }

  final class Cons[+T](h: T, t: => LazyFSeq[T]) extends LazyFSeq[T] with NonEmptySeq[T] {
    final def isDummy = false
    def next = t
    def data = h

    final override def toString = s"($h, ...)" // deliberately suppress evaluation of tail!
  }

  object Cons {
    def apply[T](h: T, t: => LazyFSeq[T]) = new Cons[T](h, t)
    def unapply[T](c: Cons[T]) = Some(c.data, c.tail)
  }

  implicit class withConsOps[T](s: => LazyFSeq[T]) {
    /** The cons operation on [[LazyFSeq]]s. */
    def ::[U >: T](a: U) = LazyFSeq.Cons(a, s)
  }

}
