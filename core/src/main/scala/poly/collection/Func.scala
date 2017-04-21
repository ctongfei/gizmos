package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.specgroup._

/**
 * Poly-collection's wrapper of [[scala.Function1]].
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Func[@sp(Int) -A, @sp(Int) +B] extends (A => B) { self =>

  import Func._

  def map[C](f: B => C): Func[A, C] = (a: A) => f(self(a))

  def mapWithKeys[A1 <: A, C](f: (A1, B) => C): Func[A1, C] = (a: A1) => f(a, self(a))

  def flatMap[A1 <: A, C](f: B => Func[A1, C]): Func[A1, C] = (a: A1) => f(self(a))(a)

  def contramap[C](f: C => A): Func[C, B] = f map self

  /** Returns the Cartesian product of two functions. */
  def product[C, D](that: C => D): Func[(A, C), (B, D)] = {
    ac: (A, C) => (self(ac._1), that(ac._2))
  }

  def zipWith[A1 <: A, C, D](that: A1 => C)(f: (B, C) => D): Func[A1, D] = (a: A1) => f(self(a), that(a))

  def zip[A1 <: A, C](that: A1 => C) = (a: A1) => (self(a), that(a))

  def ∘[C](that: C => A) = compose(that)
  def ×[C, D](that: C => D) = product(that)

}

object Func {

  def of[A, B](f: A => B): Func[A, B] = ScalaFunctionAsPoly(f)

  implicit class ScalaFunctionAsPoly[A, B](f: A => B) extends AbstractFunc[A, B] {
    def apply(a: A) = f(a)
  }

  implicit object Arrow extends Arrow[Func] {
    def id[X] = (x: X) => x
    def compose[X, Y, Z](g: Func[Y, Z], f: Func[X, Y]) = g compose f
    def lift[X, Y](f: X => Y) = f
    def apply1[X, Y, Z](f: Func[X, Y]): Func[(X, Z), (Y, Z)] = { case (x, z) => (f(x), z) }
    def apply2[X, Y, Z](f: Func[X, Y]): Func[(Z, X), (Z, Y)] = { case (z, x) => (z, f(x)) }
  }

}

abstract class AbstractFunc[-A, +B] extends Func[A, B]
