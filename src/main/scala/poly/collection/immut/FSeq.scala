package poly.collection.immut

import poly.collection._
import poly.collection.node._

/**
 * Represents a functional sequence with structural sharing.
 * The implementation is similar to [[scala.collection.immutable.List]].
 * @author Tongfei Chen
 * @since 0.1.0
 */
sealed abstract class FSeq[+T] extends AbstractSeq[T] with SeqNode[T] { self =>

  import FSeq._

  final def headNode = self

  override def map[U](f: T => U): Seq[U] with SeqNode[U] = new AbstractSeq[U] with SeqNode[U] {
    def next = self.next map f
    def data = f(self.data)
    def isDummy = self.isDummy
    def headNode = self map f
  }

  def mapE[U](f: T => U): FSeq[U] = self match {
    case Empty      => Empty
    case Cons(h, t) => Cons(f(h), t mapE f)
  }

  def next: FSeq[T]
  final override def tail = next
  final override def head = data

  /** The `Cons` operation on `FSeq`s. */
  def ::[U >: T](u: U) = Cons(u, self)

  final override def drop(n: Int): FSeq[T] =
    if (n <= 0) self
    else self match {
      case Empty      => Empty
      case Cons(h, t) => t drop (n - 1)
    }

}

object FSeq {

  case object Empty extends FSeq[Nothing] {
    final def isDummy = true
    def next = Empty
    def data = throw new NoSuchElementException
  }

  case class Cons[+T](h: T, t: FSeq[T]) extends FSeq[T] with NonEmptySeq[T] {
    final def isDummy = false
    def next = t
    def data = h
  }

}

