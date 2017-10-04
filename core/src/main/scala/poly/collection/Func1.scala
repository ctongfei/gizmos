package poly.collection

import scala.runtime._
import scala.{specialized => sp}

/**
 * Poly-collection's wrapper of [[scala.Function1]].
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Func1[@sp(Int) -A, @sp(Int) +B] extends (A => B) { self =>

  import Func1._

  def map[C](f: B => C): Func1[A, C] = (a: A) => f(self(a))

  def mapWithKeys[A1 <: A, C](f: (A1, B) => C): Func1[A1, C] = (a: A1) => f(a, self(a))

  def flatMap[A1 <: A, C](f: B => Func1[A1, C]): Func1[A1, C] = (a: A1) => f(self(a))(a)

  def contramap[C](f: C => A): Func1[C, B] = f map self

  def zipWith[A1 <: A, C, D](that: A1 => C)(f: (B, C) => D): Func1[A1, D] = (a: A1) => f(self(a), that(a))

  def zip[A1 <: A, C](that: A1 => C) = (a: A1) => (self(a), that(a))

  def ∘[C](that: C => A) = compose(that)

}

object Func1 {

  def of[A, B](f: A => B): Func1[A, B] = ScalaFunctionAsPoly(f)

  implicit class ScalaFunctionAsPoly[A, B](f: A => B) extends AbstractFunction[A, B] {
    def apply(a: A) = f(a)
  }

  implicit object Arrow extends Arrow[Func1] {
    def id[X] = (x: X) => x
    def compose[X, Y, Z](g: Func1[Y, Z], f: Func1[X, Y]) = g compose f
    def lift[X, Y](f: X => Y) = f
    def first[A, B, C](f: Func1[A, B]): Func1[(A, C), (B, C)] = { case (x, z) => (f(x), z) }
    override def second[X, Y, Z](f: Func1[X, Y]): Func1[(Z, X), (Z, Y)] = { case (z, x) => (z, f(x)) }
  }

}

abstract class AbstractFunction[-A, +B] extends AbstractFunction1[A, B] with Func1[A, B]

