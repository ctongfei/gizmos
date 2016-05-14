package poly.collection.builder

import poly.algebra.mut._
import poly.collection._
import poly.collection.mut._

import scala.annotation._
import scala.language.higherKinds

/**
  * Represents builders, which are objects that allow incremental construction
  * of other structures (e.g. collections, models).
  *
  * @author Tongfei Chen
  * @since 0.1.0
  */
@implicitNotFound("Cannot find a builder to build ${C} from elements of type ${T}.")
trait Builder[-T, +C] { // TODO: Specialize T? (Int, Float, Double, Char)

  /**
    * Provides a hint to this builder about how many elements are expected to be added.
    * @param n The hint how many elements is expected to be added
    */
  def sizeHint(n: Int)

  /**
    * Adds a single element to this builder.
    * @param x The element to be added
    */
  def addInplace(x: T)

  /**
    * Adds all elements provided to this builder.
    * @param xs The elements to be added
    */
  def addAllInplace(xs: Traversable[T]) = xs foreach addInplace

  /**
   * Returns the structure built from this builder.
   * @return A structure containing the added elements
   */
  def result: C

  def +=(x: T) = addInplace(x)
  def ++=(xs: Traversable[T]) = xs foreach addInplace

}

object Builder {

  implicit def InplaceAction[T, C]: InplaceAdditiveAction[T, Builder[T, C]] = new InplaceAdditiveAction[T, Builder[T, C]] {
    def addInplace(x: T, s: Builder[T, C]) = s addInplace x
  }

  def ofMutableSet[T, S <: KeyMutableSet[T]](s: S): Builder[T, S] = new Builder[T, S] {
    def addInplace(x: T) = s addInplace x
    def result = s
    def sizeHint(n: Int) = {}
  }

  def ofMutableMap[K, V, M <: KeyMutableMap[K, V]](m: M): Builder[(K, V), M] = new Builder[(K, V), M] {
    def addInplace(kv: (K, V)) = m addInplace kv
    def result = m
    def sizeHint(n: Int) = {}
  }

  def ofMutableSeq[T, S <: KeyMutableSeq[T]](s: S): Builder[T, S] = new Builder[T, S] {
    def addInplace(x: T) = s appendInplace x
    def result = s
    def sizeHint(n: Int) = {}
}
}
