package poly.collection.mut

import poly.collection._
import poly.collection.builder._
import poly.collection.factory._
import poly.collection.impl._
import poly.macroutil._

/**
 * A mutable sequence backed by a resizable array.
 * @since 0.1.0
 * @author Tongfei Chen
 */
class ArraySeq[T] private(private var data: ResizableSeq[T]) extends AbstractIndexedSeq[T] with ValueMutableIndexedSeq[T] with KeyMutableSeq[T] {

  def fastLength = data.fastLength

  def fastApply(i: Int) = data(i)

  def update(i: Int, x: T) = data.update(i, x)

  def insertInplace(i: Int, x: T) = data.insertInplace(i, x)

  def deleteInplace(i: Int) = data.deleteInplace(i)

  def prependInplace(x: T) = data.prependInplace(x)

  def appendInplace(x: T) = data.appendInplace(x)

  def clear() = data.clear()

}

object ArraySeq extends SeqFactory[ArraySeq] {

  implicit def newBuilder[T]: Builder[T, ArraySeq[T]] = new Builder[T, ArraySeq[T]] {
    val a = new ResizableSeq[T]()
    override def sizeHint(n: Int) = a.ensureCapacity(n)
    def addInplace(x: T) = a.appendInplace(x)
    def result = new ArraySeq[T](a)
  }

  override def tabulate[T](n: Int)(f: Int ⇒ T): ArraySeq[T] = {
    val cap = nextPowerOfTwo(n)
    val a = Array.ofDim[AnyRef](cap)
    FastLoop.ascending(0, n, 1) { i ⇒
      a(i) = f(i).asInstanceOf[AnyRef]
    }
    val rs = new ResizableSeq[T](cap); rs.data = a; rs.len = n
    new ArraySeq[T](rs)
  }

}
