package poly.collection

import poly.algebra.hkt._
import poly.collection.mut._
import poly.collection.factory._
import poly.util.specgroup._

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
  def newEnumerator: Enumerator[T]

  def foreach[V](f: T => V) = newEnumerator.foreach(f)

  override def map[U](f: T => U) = Enumerable.ofEnumerator(self.newEnumerator.map(f))

  def flatMap[U](f: T => Enumerable[U]) = Enumerable.ofEnumerator(self.newEnumerator.flatMap(x => f(x).newEnumerator))

  override def filter(f: T => Boolean) = Enumerable.ofEnumerator(self.newEnumerator.filter(f))

  def zip[U](that: Enumerable[U]): Enumerable[(T, U)] = Enumerable.ofEnumerator(self.newEnumerator zip that.newEnumerator)

  def zip3[U, V](us: Enumerable[U], vs: Enumerable[V]): Enumerable[(T, U, V)] = Enumerable.ofEnumerator {
    new Enumerator[(T, U, V)] {
      val ti = self.newEnumerator
      val ui = us.newEnumerator
      val vi = vs.newEnumerator
      def advance(): Boolean = ti.advance() && ui.advance() && vi.advance()
      def current: (T, U, V) = (ti.current, ui.current, vi.current)
    }
  }

  override def toString = this.take(Settings.MaxElemToString).buildString(", ")

}

object Enumerable {

  /** Creates an Enumerable based on an existing enumerator. */
  def ofEnumerator[T](e: Enumerator[T]): Enumerable[T] = new Enumerable[T] {
    def newEnumerator = e
  }

  /** Returns the natural monad on Enumerables. */
  implicit object Monad extends Monad[Enumerable] {
    def flatMap[X, Y](mx: Enumerable[X])(f: (X) => Enumerable[Y]): Enumerable[Y] = mx.flatMap(f)
    def id[X](u: X): Enumerable[X] = ListSeq(u)
  }
}