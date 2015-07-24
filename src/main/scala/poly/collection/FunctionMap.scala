package poly.collection

import poly.algebra.hkt._
import scala.language.reflectiveCalls

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
trait FunctionMap[-K, +V] extends (K => V) { self =>

  def mapValues[W](f: V => W): FunctionMap[K, W] = new FunctionMap[K, W] {
    def apply(k: K) = f(self(k))
  }

  def contramapKeys[J](f: J => K): FunctionMap[J, V] = new FunctionMap[J, V] {
    def apply(j: J) = self(f(j))
  }

}

object FunctionMap {

  implicit def Functor[K]: Functor[({type λ[V] = FunctionMap[K, V]})#λ] =
    new Functor[({type λ[V] = FunctionMap[K, V]})#λ] {
      def map[X, Y](mx: FunctionMap[K, X])(f: X => Y): FunctionMap[K, Y] = mx mapValues f
    }
  
  //TODO: ContravariantFunctor
}