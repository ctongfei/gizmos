package poly.collection.mut

import poly.algebra.specgroup._
import poly.collection._

/**
 * @author Tongfei Chen
 */
trait KeyMutableSet[@sp(Int) T] extends Set[T] {

  def add_!(x: T)

  def remove_!(x: T)

  def clear_!()

  def addAll_!(xs: Traversable[T]) = xs foreach add_!

  def removeAll_!(xs: Traversable[T]) = xs foreach remove_!

  def union_!(xs: Set[T]) = xs.elements foreach add_!

  def diff_!(xs: Set[T]) = xs.elements foreach remove_!

  def intersect_!(xs: Set[T]) = this.elements filter xs.notContains foreach remove_!

  @inline final def +=(x: T) = add_!(x)

  @inline final def -=(x: T) = remove_!(x)

  @inline final def ++=(xs: Traversable[T]) = addAll_!(xs)

  @inline final def --=(xs: Traversable[T]) = removeAll_!(xs)

  @inline final def |=(xs: Set[T]) = union_!(xs)

  @inline final def &=(xs: Set[T]) = intersect_!(xs)

  @inline final def ∪=(xs: Set[T]) = union_!(xs)

  @inline final def ∩=(xs: Set[T]) = intersect_!(xs)


}
