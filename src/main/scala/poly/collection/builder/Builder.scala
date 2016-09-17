package poly.collection.builder

import poly.algebra.mut._
import poly.algebra.specgroup._
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
trait Builder[-T, +C] { self =>

  /**
    * Provides a hint to this builder about how many elements are expected to be added.
    * @param n The hint how many elements is expected to be added
    */
  def sizeHint(n: Int) = {}

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

  @inline final def +=(x: T) = addInplace(x)

  @inline final def ++=(xs: Traversable[T]) = xs foreach addInplace

  /**
   * Returns a new builder which wraps around this builder. The difference
   * is that the result is mapped by the specified function.
   */
  def map[D](f: C => D): Builder[T, D] = new Builder[T, D] {
    def addInplace(x: T) = self addInplace x
    def result = f(self.result)
    override def sizeHint(n: Int) = self sizeHint n
  }

  def |>[D](f: C => D) = map(f)

}

object Builder {

  implicit def InplaceAction[T, C]: InplaceAdditiveAction[T, Builder[T, C]] = new InplaceAdditiveAction[T, Builder[T, C]] {
    def addInplace(x: T, s: Builder[T, C]) = s addInplace x
  }

  def ofMutableSet[T, S <: KeyMutableSet[T]](s: S): Builder[T, S] = new Builder[T, S] {
    def addInplace(x: T) = s addInplace x
    def result = s
  }

  def ofMutableMap[K, V, M <: KeyMutableMap[K, V]](m: M): Builder[(K, V), M] = new Builder[(K, V), M] {
    def addInplace(kv: (K, V)) = m addInplace kv
    def result = m
  }

  def ofMutableSeq[T, S <: KeyMutableSeq[T]](s: S): Builder[T, S] = new Builder[T, S] {
    def addInplace(x: T) = s appendInplace x
    def result = s
}
}
