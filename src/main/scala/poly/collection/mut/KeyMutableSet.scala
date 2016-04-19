package poly.collection.mut

import poly.algebra.specgroup._
import poly.collection._

/**
 * @author Tongfei Chen
 */
trait KeyMutableSet[@sp(Int) T] extends Set[T] {

  def add(x: T)

  def remove(x: T)

  def clear()

  def addAllInplace(xs: Traversable[T]) = xs foreach add

  def unionInplace(xs: Set[T]) = xs.elements foreach add

  def diffInplace(xs: Set[T]) = xs.elements foreach remove

  def intersectInplace(xs: Set[T]) = this.elements filter xs.notContains foreach remove

}
