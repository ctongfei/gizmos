package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.collection.exception._
import poly.collection.mut._
import poly.collection.node._

import scala.reflect._

/**
 * @author Tongfei Chen
 */
trait SortedSeq[T] extends Seq[T] with SortedIterable[T] { self =>

  override def distinct: SortedSeq[T] = self

  def asSortedSet = ???

  def thenSortBy[X](f: T => X)(implicit subOrder: WeakOrder[T]): SortedIndexedSeq[T] = {
    ???
  }

}
