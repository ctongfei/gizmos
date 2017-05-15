package poly.collection

import poly.collection.specgroup._
import poly.macroutil._

import scala.annotation.unchecked.{uncheckedVariance => uv}
import scala.language.higherKinds

/**
 * Represents builders, which are objects that allow incremental construction
 * of other structures (e.g. collections, models, files).
 *
 * It can be considered as a sink that drains streams of data, and acts as a
 * categorical dual of [[Iterator]]s.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Builder[@sp(Int, Byte, Char) -T, @sp(Unit, AnyRef) +R] { self =>

  /**
    * Provides a hint to this builder about how many elements are expected to be added.
    * @param n The hint how many elements is expected to be added
    */
  def sizeHint(n: Int) = {}

  /**
    * Adds a single element to this builder.
    * @param x The element to be added
    */
  def add(x: T)

  /**
    * Adds all elements provided to this builder.
    * @param xs The elements to be added
    */
  @unsp def addAll(xs: Traversable[T]) = {
    if (xs.sizeKnown) sizeHint(xs.size)
    xs foreach add
  }

  /**
   * Returns the structure built from this builder.
   * @note This operation may incur side effects.
   *       The builder should not be used after [[result]] is called.
   * @return A structure containing the added elements
   */
  def result(): R

  def writeFromArray(a: Array[T @uv], off: Int, len: Int): Unit = {
    FastLoop.ascending(off, off + len, 1) { i =>
      self.add(a(i))
    }
  }

  def writeFromArray(a: Array[T @uv]): Unit = writeFromArray(a, 0, a.length)

  /**
   * Adds a single element to this builder.
   */
  @inline final def <<(x: T): this.type = { add(x); self }

  /**
   * Adds a traversable of elements to this builder.
   */
  @unsp @inline final def <<<(xs: Traversable[T]): this.type = { addAll(xs); self }

  @inline final def <<<(xs: Iterator[T]): this.type = { xs >>> this; self }

  @inline final def <<<!(xs: Iterator[T]): R = { xs >>> this; this.result() }

  /**
   * Adds a traversable of elements to this builder and the closes it, returning the built value.
   */
  @unsp @inline final def <<<!(xs: Traversable[T]): R = (this <<< xs).result()

  /**
   * Returns a new builder which wraps around this builder. The difference
   * is that the result is mapped by the specified function.
   */
  @unsp def map[S](f: R => S): Builder[T, S] = new BuilderT.Mapped(self, f)

  /**
   * Returns a new builder in which every element added will be transformed
   * by the given function.
   */
  @unsp def contramap[S](f: S => T): Builder[S, R] = new BuilderT.Contramapped(self, f)

}

private[poly] object BuilderT {

  class Mapped[T, R, S](self: Builder[T, R], f: R => S) extends Builder[T, S] {
    def add(x: T) = self add x
    def result = f(self.result)
    override def sizeHint(n: Int) = self sizeHint n
  }

  class Contramapped[S, T, R](self: Builder[T, R], f: S => T) extends Builder[S, R] {
    def add(x: S) = self add f(x)
    def result = self.result
    override def sizeHint(n: Int) = self sizeHint n
  }

}
