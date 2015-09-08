package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.collection.exception._
import poly.collection.node._
import poly.util.typeclass._

/**
 * Trait for sequences.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Seq[+T] extends Iterable[T] with Map[Int, T] { self =>

  import Seq._

  /** Returns the head node of this sequence. If the sequence is empty, returns a dummy. */
  def headNode: SeqNode[T]

  /**
   * Returns the length of this sequence.
   * @return The length of this sequence
   */
  def length: Int

  /**
   * Returns the ''i''-th element of this sequence.
   * @param i Index
   * @return The ''i''-th element of this sequence
   */
  def apply(i: Int): T

  override def size = length

  override def foreach[V](f: T => V): Unit = {
    var node = headNode
    while (node.notDummy) {
      f(node.data)
      node = node.next
    }
  }

  def newIterator: Iterator[T] = new AbstractIterator[T] {
    var node: SeqNode[T] = SeqNode.dummy
    var first = true
    def advance() = {
      if (first) {
        first = false
        node = headNode
      }
      else node = node.next
      node.notDummy
    }
    def current = node.data
  }


  override def isDefinedAt(i: Int) = containsKey(i)

  def ?(i: Int) = if (containsKey(i)) Some(this(i)) else None

  def containsKey(i: Int) = i >= 0 && i < size

  def pairs = {
    var i = -1
    self.map(x => { i += 1; (i, x) })
  }

  def asLinearSeq = LinearSeq.ofNode(self.headNode)

  // HELPER FUNCTIONS

  override def isEmpty = headNode.isDummy

  override def head = headNode.data

  override def tail = asLinearSeq.tail

  override def map[U](f: T => U): Seq[U] = asLinearSeq.map(f)

  def flatMap[U](f: T => Seq[U]): Seq[U] = asLinearSeq.flatMap(f)

  override def filter(f: T => Boolean): Seq[T] = asLinearSeq.filter(f)

  def concat[U >: T](that: Seq[U]): Seq[U] = asLinearSeq.concat(that)

  override def prepend[U >: T](x: U): Seq[U] = asLinearSeq.prepend(x)

  override def append[U >: T](x: U): Seq[U] = asLinearSeq.append(x)

  override def foldRight[U](z: U)(f: (T, U) => U): U = asLinearSeq.foldRight(z)(f)

  /**
   * Pretends that this sequence is sorted under the given order.
   * @param O The implicit order
   * @return A sorted order (WARNING: Actual orderedness is not guaranteed! The user should make sure that it is sorted.)
   */
  def asIfSorted[U >: T](implicit O: WeakOrder[U]): SortedSeq[U] = new SortedSeq[U] {
    val order: WeakOrder[U] = O
    def headNode: SeqNode[T] = self.headNode
    def apply(i: Int) = self.apply(i)
    def length = self.length
  }

  override def equals(that: Any) = that match {
    case (that: Seq[T]) => Eq[T].eq(this, that)
    case _ => false
  }

  override def toString = "(" + buildString(",") + " #)" // overridden the `toString` in Map

  override def hashCode = ???

}

object Seq {

  def unapply[T](xs: Seq[T]) = {
    if (xs.isEmpty) None
    else Some((xs.head, xs.tail))
  }

  object empty extends Seq[Nothing] {
    override def apply(i: Int): Nothing = throw new NoSuchElementException
    override def length: Int = 0
    def headNode: SeqNode[Nothing] = throw new NoSuchElementException
  }

  implicit def Eq[T: Eq]: Eq[Seq[T]] = new Eq[Seq[T]] {
    def eq(x: Seq[T], y: Seq[T]): Boolean = {
      val xi = x.newIterator
      val yi = y.newIterator
      while (xi.advance() && yi.advance())
        if (xi.current =!= yi.current) return false
      if (xi.advance()) return false
      if (yi.advance()) return false
      true
    }
  }


  def tabulate[T](n: Int)(f: Int => T) = IndexedSeq.tabulate(n)(f)

}

abstract class AbstractSeq[+T] extends AbstractIterable[T] with Seq[T]
