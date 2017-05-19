package poly.collection.factory

import cats.data._
import scala.language.higherKinds

/**
 * A factory trait for all collections that can be constructed
 * as a lazy sequence.
 * @author Tongfei Chen
 * @define LAZY '''[LAZY]'''
 */
trait UnfoldFactory[CC[+_]] {

  /** $LAZY
   * Given a starting state [[s0]], infinitely unfolds into an iterable sequence using the given stateful transition function.
   */
  def unfold[S, T](s0: S)(f: S => (S, T)): CC[T]

  /** $LAZY
   * Given a starting state [[s0]], infinitely unfolds into an iterable sequence using the given [[cats.data.State]] monad instance.
   */
  def unfoldByState[S, T](s0: S)(f: State[S, T]) = unfold(s0)(s => f.run(s).value)

  //def unfoldUntil[S, T](s0: S, sf: S => Boolean)(f: S => (S, T)): CC[T]

  /** $LAZY
   * Iterates infinitely by recursively applying a function to a starting value.
   * @example {{{
   *   iterate(1)(_+1) == (1, 2, 3, 4, ...)
   * }}}
   */
  def iterate[T](t0: T)(f: T => T): CC[T] = unfold(t0)(s => (f(s), s))

  def infinite[T](t: => T): CC[T] = unfold(())(_ => ((), t))

}
