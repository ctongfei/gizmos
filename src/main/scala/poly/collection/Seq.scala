package poly.collection

import poly.algebra._
import scala.annotation.unchecked.{uncheckedVariance => uV}

/**
 * Trait for sequences.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Seq[+T] extends (Int =!> T) with Enumerable[T] { self =>

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

  def isDefinedAt(i: Int) = i >= 0 && i < size

  /**
   * Pretends that this sequence is sorted under the given order.
   * @param O The implicit order
   * @return A sorted order (WARNING: Actual orderedness is not guaranteed! The user should make sure that it is sorted.)
   */
  def asIfSorted(implicit O: WeakOrder[T @uV]): SortedSeq[T @uV] = new SortedSeq[T] {
    val order: WeakOrder[T] = O
    def length: Int = self.length
    def apply(i: Int): T = self.apply(i)
    def enumerator: Enumerator[T] = self.enumerator
  }

  override def equals(that: Any) = that match {
    case that @ (other: Seq[T]) => ??? //TODO!!!
    case _ => false
  }

  override def toString() = {
    val len = length
    if (len > Settings.MaxElemToString)
      s"[$len] " + this.take(Settings.MaxElemToString).mkString(", ") + "..."
    else s"[$len] " + this.mkString(", ")
  }

  override def hashCode = ???



}
