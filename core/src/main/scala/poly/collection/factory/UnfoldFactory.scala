package poly.collection.factory

import scala.language.higherKinds

/**
 * @author Tongfei Chen
 * @define LAZY '''[LAZY]'''
 */
trait UnfoldFactory[CC[+_]] {

  /** $LAZY
   * Given a starting state [[s0]], infinitely unfolds into an iterable sequence using the given stateful transition function.
   */
  def unfold[S, T](s0: S)(f: S => (S, T)): CC[T]

  def iterate[T](t0: T)(f: T => T): CC[T] = unfold(t0)(s => (s, s))

  def infinite[T](t: => T): CC[T] = unfold(())(_ => ((), t))

}
