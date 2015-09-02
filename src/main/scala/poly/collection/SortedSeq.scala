package poly.collection

import poly.algebra._
import poly.algebra.ops._
import poly.collection.exception._
import poly.collection.mut._
import poly.collection.node._

import scala.reflect._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait SortedSeq[T] extends Seq[T] with SortedIterable[T] { self =>

  override def distinct: SortedSeq[T] = ???

  def thenSortBy[X](f: T => X)(implicit subOrder: WeakOrder[T]): IndexedSortedSeq[T] = {
    ???
  }

}
