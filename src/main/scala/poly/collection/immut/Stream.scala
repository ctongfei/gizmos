package poly.collection.immut

import poly.collection._
import poly.collection.node._

/**
 * Represents a lazy stream in which traversed elements will be cached in a manner similar to a linked list.
 * @author Tongfei Chen
 * @since 0.1.0
 */
sealed abstract class Stream[+T] extends AbstractSeq[T] with SeqNode[T] { self =>

  import Stream._

  final def headNode = self

  override def map[U](f: T => U): Stream[U] = self match {
    case Empty      => Empty
    case Cons(h, t) => Cons(f(h), t map f)
  }

  def next: Stream[T]
  final override def tail = next
  final override def head = data

  final override def drop(n: Int): Stream[T] =
    if (n <= 0) self
    else self match {
      case Empty      => Empty
      case Cons(h, t) => t drop (n - 1)
    }

}

object Stream {

  case object Empty extends Stream[Nothing] {
    final def isDummy = true
    def next = Empty
    def data = throw new NoSuchElementException

    final override def toString = "()"
  }

  final class Cons[+T](h: T, t: => Stream[T]) extends Stream[T] {
    final def isDummy = false
    /*
    @volatile private[this] var tVal: Stream[T] = null
    @volatile private[this] var tGen = t _
    def next = {
      if (tGen ne null) {
        synchronized {
          if (tGen ne null) {
            tVal = tGen()
            tGen = null
          }
        }
      }
      tVal
    }*/
    def next = t
    def data = h

    final override def toString = s"($h, ...)" // suppress evaluation!
  }

  object Cons {
    def apply[T](h: T, t: => Stream[T]) = new Cons[T](h, t)
    def unapply[T](c: Cons[T]) = Some(c.data, c.tail)
  }

  implicit class withConsOps[T](s: => Stream[T]) {
    /** The cons operation on [[Stream]]s. */
    def ::[U >: T](a: U) = Stream.Cons(a, s)
  }

}
