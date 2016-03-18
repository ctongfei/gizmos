package poly.collection.mut

import poly.collection._

/**
 * @author Tongfei Chen
 */
trait KeyMutableSet[T] extends Set[T] {

  def add(x: T)

  def remove(x: T)

  def clear()

  def unionInplace(xs: Set[T]) = xs.elements foreach add

  def diffInplace(xs: Set[T]) = xs.elements foreach remove

  def intersectInplace(xs: Set[T]) = this.elements filter xs.notContains foreach remove

}
