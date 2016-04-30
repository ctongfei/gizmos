package poly.collection.builder

import poly.collection._

/**
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait RemovableBuilder[-T, +C] extends Builder[T, C] {

  def remove(x: T): Unit

  def removeAll(xs: Traversable[T]) = xs foreach remove

  def -=(x: T) = remove(x)

  def --=(xs: Traversable[T]) = xs foreach remove

}
