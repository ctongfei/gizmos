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
class ArraySeq[T] private(private[poly] var data: ResizableSeq[T]) extends AbstractIndexedSeq[T] with ValueMutableIndexedSeq[T] with KeyMutableSeq[T] {

  def fastLength = data.fastLength

  def fastApply(i: Int) = data(i)

  def update(i: Int, x: T) = data.update(i, x)

  def insert_!(i: Int, x: T) = data.insert_!(i, x)

  def delete_!(i: Int) = data.delete_!(i)

  def prepend_!(x: T) = data.prepend_!(x)

  def append_!(x: T) = data.append_!(x)

  def clear_!() = data.clear_!()

}

object ArraySeq extends SeqFactory[ArraySeq] {

  implicit def newBuilder[T]: Builder[T, ArraySeq[T]] = new Builder[T, ArraySeq[T]] {
    val a = new ResizableSeq[T]()
    override def sizeHint(n: Int) = a.ensureCapacity(n)
    def add(x: T) = a.append_!(x)
    def result = new ArraySeq[T](a)
  }

  override def tabulate[T](n: Int)(f: Int => T): ArraySeq[T] = {
    val cap = nextPowerOfTwo(n)
    val a = Array.ofDim[AnyRef](cap)
    FastLoop.ascending(0, n, 1) { i =>
      a(i) = f(i).asInstanceOf[AnyRef]
    }
    val rs = new ResizableSeq[T](cap); rs.data = a; rs.len = n
    new ArraySeq[T](rs)
  }

}
