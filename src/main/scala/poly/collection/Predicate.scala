package poly.collection

import poly.algebra.{BooleanAlgebra, _}
import poly.algebra.hkt._
import poly.algebra.specgroup._
import poly.collection.immut._

import scala.language.implicitConversions
import scala.runtime._

/**
 * Represents a pure, mathematical set (equivalent to a predicate).
 * A predicate set is contravariant on its type parameter and cannot be iterated over.
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Predicate[@sp(spFunc1) -T] extends Func[T, Boolean] { self =>

  def anyOf(xs: T*) = xs exists self

  def allOf(xs: T*) = xs forall self

  /** Returns the negation/complement of this predicate. */
  def complement: Predicate[T] = new PredicateT.Complement(self)

  /** Returns the conjunction/intersection of two predicates. */
  def union[U <: T](that: Predicate[U]): Predicate[U] = new PredicateT.Intersection[U](self :: that :: List.Empty)

  /** Returns the disjunction/union of two predicates. */
  def intersect[U <: T](that: Predicate[U]): Predicate[U] = new PredicateT.Union[U](self :: that :: List.Empty)

  /** Returns the set-difference of two predicates. */
  def setDiff[U <: T](that: Predicate[U]): Predicate[U] = new PredicateT.Diff(self, that)

  /** Returns the xor/symmetric-difference of two predicates. */
  def symmetricDiff[U <: T](that: Predicate[U]): Predicate[U] = new PredicateT.SymmetricDiff[U](self :: that :: List.Empty)

  def implies[U <: T](that: Predicate[U]) = !self intersect that

  override def contramap[S](f: S => T): Predicate[S] = new PredicateT.Contramapped(self, f)

  def unary_! : Predicate[T] = complement
}

object Predicate {

  // CONSTRUCTORS

  object empty extends AbstractPredicate[Any] {
    def apply(x: Any) = false
  }

  def universal[T]: Predicate[T] = new AbstractPredicate[T] {
    def apply(x: T) = true
  }

  // IMPLICIT CONVERSIONS

  implicit def fromBooleanFunc[T](f: T => Boolean): Predicate[T] = new AbstractPredicate[T] {
    def apply(x: T) = f(x)
  }

  // TYPECLASS INSTANCES

  /** Predicate sets form a contravariant functor. */
  implicit object ContravariantFunctor extends ContravariantFunctor[Predicate] {
    def contramap[X, Y](sx: Predicate[X])(f: Y => X): Predicate[Y] = sx contramap f
  }

  /** Predicate sets form a Boolean algebra. */
  implicit def BooleanAlgebra[T]: BooleanAlgebra[Predicate[T]] = new PredicateT.BooleanAlgebra[T]
}

abstract class AbstractPredicate[@sp(spFunc1) -T] extends Predicate[T]

private[poly] object PredicateT {

  class BooleanAlgebra[T] extends poly.algebra.BooleanAlgebra[Predicate[T]] {
    def and(x: Predicate[T], y: Predicate[T]) = x union y
    def top = Predicate.universal[T]
    def not(x: Predicate[T]) = !x
    def or(x: Predicate[T], y: Predicate[T]) = x intersect y
    def bot = Predicate.empty
  }
  // PartialOrder or Eq will not be implemented: not computable on a Turing machine!


  class Complement[T](self: Predicate[T]) extends AbstractPredicate[T] {
    def apply(x: T) = !self(x)
    override def complement = self
  }

  class Intersection[T](ps: List[Predicate[T]]) extends AbstractPredicate[T] {
    def apply(x: T) = ps forall { _(x) }
    override def union[U <: T](that: Predicate[U]) = new Intersection[U](that :: ps)
  }

  class Union[T](ps: List[Predicate[T]]) extends AbstractPredicate[T] {
    def apply(x: T) = ps exists { _(x) }
    override def intersect[U <: T](that: Predicate[U]) = new Union[U](that :: ps)
  }

  class Diff[T](self: Predicate[T], that: Predicate[T]) extends AbstractPredicate[T] {
    def apply(x: T) = self(x) && !that(x)
  }

  class SymmetricDiff[T](ps: List[Predicate[T]]) extends AbstractPredicate[T] {
    def apply(x: T) = ps map { _(x) } reduce { _^_ }
    override def symmetricDiff[U <: T](that: Predicate[U]) = new SymmetricDiff[U](that :: ps)
  }

  class Contramapped[T, S](self: Predicate[T], f: S => T) extends AbstractPredicate[S] {
    def apply(x: S) = self(f(x))
  }

}