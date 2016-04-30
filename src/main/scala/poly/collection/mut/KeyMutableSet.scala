package poly.collection.mut

import poly.algebra.specgroup._
import poly.collection._

/**
 * @author Tongfei Chen
 */
trait KeyMutableSet[@sp(Int) T] extends Set[T] {

  def addInplace(x: T)

  def removeInplace(x: T)

  def clear()

  def addAllInplace(xs: Traversable[T]) = xs foreach addInplace

  def unionInplace(xs: Set[T]) = xs.elements foreach addInplace

  def diffInplace(xs: Set[T]) = xs.elements foreach removeInplace

  def intersectInplace(xs: Set[T]) = this.elements filter xs.notContains foreach removeInplace

}
