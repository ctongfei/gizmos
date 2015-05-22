package poly.collection

import poly.algebra.hkt._
import poly.collection.mut._

/**
 * `Enumerable` is the basic trait for all collections that exposes an enumerator.
 * `Enumerable`s differ from `Traversable`s in that the iteration process can be controlled:
 * It can be paused or resumed by the user.
 *
 * This trait is created to replace the `Iterable` Java interface or the `Iterable` Scala
 * trait in Poly Collection.
 *
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait Enumerable[+T] extends Traversable[T] { self =>

  /** Returns a new enumerator that can be used to iterate through this collection. */
  def enumerator: Enumerator[T]

  def foreach[V](f: T => V) = enumerator.foreach(f)

  override def map[U](f: T => U) = new Enumerable[U] {
    def enumerator = self.enumerator.map(f)
  }

  def flatMap[U](f: T => Enumerable[U]) = new Enumerable[U] {
    def enumerator = self.enumerator.flatMap(x => f(x).enumerator)
  }

  override def filter(f: T => Boolean) = new Enumerable[T] {
    def enumerator = self.enumerator.filter(f)
  }

  def zip[U](that: Enumerable[U]): Enumerable[(T, U)] = new Enumerable[(T, U)] {
    def enumerator: Enumerator[(T, U)] = new Enumerator[(T, U)] {
      val ti = self.enumerator
      val ui = that.enumerator
      def advance(): Boolean = ti.advance() && ui.advance()
      def current: (T, U) = (ti.current, ui.current)
    }
  }

}

object Enumerable {
  ///** Returns the natural monad on Enumerables. */
  //implicit def monad[T]: Monad[Enumerable] = new Monad[Enumerable] {
  //  def flatMap[X, Y](mx: Enumerable[X])(f: (X) => Enumerable[Y]): Enumerable[Y] = mx.flatMap(f)
  //  def id[X](u: X): Enumerable[X] = ListSeq(u)
  //}
}