package poly.collection

import scala.language.higherKinds

/**
 * The base trait for builders, which are objects that allow
 * incremental construction of other structures (e.g. collections, models).
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
trait Builder[-T, +C] {

  /**
   * Provides a hint to this builder about how many elements are expected to be added.
   * @param n The hint how many elements is to be added
   */
  def sizeHint(n: Int)

  /**
   * Adds a single element to this builder.
   * @param x The element to be added
   */
  def +=(x: T)

  /**
   * Adds all elements provided to this builder.
   * @param xs The elements to be added
   */
  def ++=(xs: Traversable[T]) = xs foreach +=

  /**
   * Returns the structure built from this builder.
   * @return A structure containing the elements added
   */
  def result: C
}
