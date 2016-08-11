package poly.collection

import poly.algebra._
import poly.algebra.specgroup._
import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Represents a map that is sorted by key when enumerated.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait SortedMap[@sp(Int) K, +V] extends Map[K, V] { self =>

  def keySet: SortedSet[K]

  /** Returns the weak order on keys. */
  def keyOrder = keySet.keyOrder

  override def keys = keySet.keys

  override def pairs: SortedIterable[(K, V @uv)] = keySet.keys.map(k => (k, apply(k))).asIfSorted(keyOrder contramap first)

}

abstract class AbstractSortedMap[K, +V] extends AbstractMap[K, V] with SortedMap[K, V]

// TODO: This trait is a workaround to enforce that the case K=Int in `Seq[V] extends SortedMap[K, V]` is specialized.
// TODO: I don't know why specialization does not work if I write `Seq[T] extends SortedMap[Int, T]`.
private[poly] trait IntKeyedSortedMap[+V] extends SortedMap[Int, V]

