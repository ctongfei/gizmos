package poly.collection.immut

import poly.collection._
import poly.collection.builder._
import poly.collection.factory._
import poly.collection.node._
import scala.annotation._

/**
 * Represents a functional sequence with structural sharing.
 * The implementation is similar to [[scala.collection.immutable.List]].
 * @author Tongfei Chen
 * @since 0.1.0
 */
sealed abstract class List[+T] extends AbstractSeq[T] with SeqNodeLike[T, List[T]] with SeqNode[T] { self =>

  import List._

  final def headNode = self

  /** The eager version of `map`. */
  def mapE[U](f: T => U): List[U] = self match {
    case Empty      => Empty
    case Cons(h, t) => Cons(f(h), t mapE f)
  }

  def next: List[T]
  final override def tail = next
  final override def head = data

  /** The `Cons` operation on `List`s. */
  def ::[U >: T](u: U) = Cons(u, self)

  @tailrec final override def drop(n: Int): List[T] =
    if (n <= 0) self
    else self match {
      case Empty      => Empty
      case Cons(h, t) => t drop (n - 1)
    }

  override def toString = super[AbstractSeq].toString
}

object List extends SeqFactory[List] {

  implicit def newBuilder[T]: Builder[T, List[T]] = new Builder[T, List[T]] { //TODO: Wrong!
    private[this] var head: List[T] = List.Empty
    private[this] var last = head
    def add(x: T) = head = Cons(x, head)
    def result = head
  }

  case object Empty extends List[Nothing] {
    def isDummy = true
    def next = Empty
    def data = throw new NoSuchElementException
  }

  case class Cons[+T](h: T, t: List[T] = Empty) extends List[T] with NonEmptySeq[T] {
    final def isDummy = false
    def next = t
    def data = h
  }

}

