package poly.collection

import poly.algebra._
import poly.algebra.hkt._
import poly.algebra.specgroup._

import scala.language.implicitConversions

/**
 * Represents a pure, mathematical set (equivalent to a predicate).
 * A predicate set is contravariant on its type parameter and cannot be iterated.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Predicate[@sp(Int) -T] extends (T => Boolean) { self =>

  /** Returns the negation/complement of this predicate. */
  def unary_! : Predicate[T] = new PredicateT.Complement(self)

  /** Returns the conjunction/intersection of two predicates. */
  def union[U <: T](that: Predicate[U]): Predicate[U] = new PredicateT.Intersection[U](Seq(self, that)) //TODO: ImList?

  /** Returns the disjunction/union of two predicates. */
  def intersect[U <: T](that: Predicate[U]): Predicate[U] = new PredicateT.Union[U](Seq(self, that))

  /** Returns the set-difference of two predicates. */
  def setDiff[U <: T](that: Predicate[U]): Predicate[U] = new PredicateT.Diff(self, that)

  /** Returns the xor/symmetric-difference of two predicates. */
  def symmetricDiff[U <: T](that: Predicate[U]): Predicate[U] = new PredicateT.SymmetricDiff[U](Seq(self, that))

  def implies[U <: T](that: Predicate[U]) = !self intersect that

  def contramap[S](f: S => T): Predicate[S] = new PredicateT.Contramapped(self, f)
}

object Predicate {

  // CONSTRUCTORS

  object empty extends Predicate[Any] {
    def apply(x: Any) = false
  }

  def universal[T]: Predicate[T] = new Predicate[T] {
    def apply(x: T) = true
  }

  // IMPLICIT CONVERSIONS

  implicit def fromBooleanFunc[T](f: T => Boolean): Predicate[T] = new Predicate[T] {
    def apply(x: T) = f(x)
  }

  // TYPECLASS INSTANCES

  /** Predicate sets form a contravariant functor. */
  implicit object ContravariantFunctor extends ContravariantFunctor[Predicate] {
    def contramap[X, Y](sx: Predicate[X])(f: Y => X): Predicate[Y] = sx contramap f
  }

  /** Predicate sets form a Boolean algebra. */
  implicit def BooleanAlgebra[T]: BooleanAlgebra[Predicate[T]] = new BooleanAlgebra[Predicate[T]] {
    def and(x: Predicate[T], y: Predicate[T]) = x union y
    def top = Predicate.universal[T]
    def not(x: Predicate[T]) = !x
    def or(x: Predicate[T], y: Predicate[T]) = x intersect y
    def bot = Predicate.empty
  }
  // Order or Eq will not be implemented: not computable on a Turing machine!

}

private[poly] object PredicateT {

  class Complement[T](self: Predicate[T]) extends Predicate[T] {
    def apply(x: T) = !self(x)
  }

  class Intersection[T](ps: Seq[Predicate[T]]) extends Predicate[T] {
    def apply(x: T) = ps forall { p => p(x) }
    override def union[U <: T](that: Predicate[U]) = new Intersection[U](that +: ps)
  }

  class Union[T](ps: Seq[Predicate[T]]) extends Predicate[T] {
    def apply(x: T) = ps exists { p => p(x) }
    override def intersect[U <: T](that: Predicate[U]) = new Union[U](that +: ps)
  }

  class Diff[T](self: Predicate[T], that: Predicate[T]) extends Predicate[T] {
    def apply(x: T) = self(x) && !that(x)
  }

  class SymmetricDiff[T](ps: Seq[Predicate[T]]) extends Predicate[T] {
    def apply(x: T) = ps map { p => p(x) } reduce { _^_ }
    override def symmetricDiff[U <: T](that: Predicate[U]) = new SymmetricDiff[U](that +: ps)
  }

  class Contramapped[T, S](self: Predicate[T], f: S => T) extends Predicate[S] {
    def apply(x: S) = self(f(x))
  }

}