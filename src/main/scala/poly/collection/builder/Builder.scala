package poly.collection.builder

import poly.collection._

import scala.language.higherKinds

/**
  * The base trait for builders, which are objects that allow
  * incremental construction of other structures (e.g. collections, models).
  *
  * @author Tongfei Chen
  * @since 0.1.0
  */
@scala.annotation.implicitNotFound("Cannot find the builder to build ${C} from ${T}.")
trait Builder[-T, +C] {

  /**
    * Provides a hint to this builder about how many elements are expected to be added.
    * @param n The hint how many elements is expected to be added
    */
  def sizeHint(n: Int)

  /**
    * Adds a single element to this builder.
    * @param x The element to be added
    */
  def add(x: T)

  /**
    * Adds all elements provided to this builder.
    * @param xs The elements to be added
    */
  def addAll(xs: Traversable[T]) = xs foreach add

  /**
   * Returns the structure built from this builder.
   * @return A structure containing the elements added
   */
  def result: C

  def +=(x: T) = add(x)
  def ++=(xs: Traversable[T]) = xs foreach add
}

object Builder {
  //implicit def inplaceAdditiveAction?
}