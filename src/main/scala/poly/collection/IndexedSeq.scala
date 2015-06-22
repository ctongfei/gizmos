package poly.collection

import poly.algebra._
import poly.collection.factory._
import poly.collection.mut._
import poly.collection.node._
import poly.util.specgroup._
import scala.annotation.unchecked.{uncheckedVariance => uV}
import scala.reflect._

/**
 * Basic trait for indexed sequences. Indexed sequences should support O(1) random access.
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait IndexedSeq[@sp(fdi) +T] extends Seq[T] { self =>

  def apply(i: Int): T

  override def newEnumerator: Enumerator[T] = new Enumerator[T] {
    private[this] var i: Int = -1
    def current = self(i)
    def advance(): Boolean = {
      i += 1
      i < self.size
    }
  }

  def headNode: BidiSeqNode[T] = new IndexedSeqNode(0)
  def lastNode: BidiSeqNode[T] = new IndexedSeqNode(length - 1)

  // HELPER FUNCTIONS

  /**
   * Rotates this sequence from the index specified.
   * @param j Rotation starts here
   * @return
   */
  def rotate(j: Int): IndexedSeq[T] = new IndexedSeq[T] {
    val len = self.length
    def apply(i: Int): T = self((j + i) % len)
    def length: Int = len
  }

  /**
   * Pretends that this sequence is sorted under the given order.
   * (WARNING: Actual orderedness is not guaranteed! The user should make sure that it is sorted.)
   * @param O The implicit order
   * @return A sorted order
   */
  override def asIfSorted[U >: T](implicit O: WeakOrder[U]): SortedIndexedSeq[U] = new SortedIndexedSeq[U] {
    val order: WeakOrder[U] = O
    def length: Int = self.length
    def apply(i: Int): T = self.apply(i)
  }

  class IndexedSeqNode(val i: Int) extends BidiSeqNode[T] {
    def data = self(i)
    def next = if (i == length - 1) null else new IndexedSeqNode(i + 1)
    def prev = if (i == 0) null else new IndexedSeqNode(i - 1)
  }

}

object IndexedSeq extends SeqFactoryWithTag[IndexedSeq] {

  def newBuilder[@sp(fdi) T: ClassTag] = ArraySeq.newBuilder[T]

}
