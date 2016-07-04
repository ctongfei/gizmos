package poly.collection

import poly.algebra._
import poly.collection.mut._

/**
 * Represents a binary relation between two types.
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Relation[-X, -Y] { self =>

  /** Checks if two elements are related under this binary relation. */
  def related(x: X, y: Y): Boolean

  def inverse: Relation[Y, X] = new RelationT.Inverse(self)

  def complement: Relation[X, Y] = new RelationT.Complement(self)

  def union[U <: X, V <: Y](that: Relation[U, V]): Relation[U, V] = new RelationT.Union(ListSeq(self, that))

  def intersect[U <: X, V <: Y](that: Relation[U, V]): Relation[U, V] = new RelationT.Intersection(ListSeq(this, that))

  def product[U, V](that: Relation[U, V]): Relation[(X, U), (Y, V)] = new RelationT.Product(self, that)

  def unary_! = complement
  def |[U <: X, V <: Y](that: Relation[U, V]) = union(that)
  def &[U <: X, V <: Y](that: Relation[U, V]) = intersect(that)
  def ×[U, V](that: Relation[U, V]) = product(that)

}

object Relation {

  implicit class FunctionAsRelation[X, Y](f: X => Y)(implicit Y: Eq[Y]) extends Relation[X, Y] {
    /** Checks if two elements is related under this binary relation. */
    def related(x: X, y: Y) = Y.eq(f(x), y)
  }

}

private[poly] object RelationT {

  class Complement[X, Y](self: Relation[X, Y]) extends Relation[X, Y] {
    def related(x: X, y: Y) = !self.related(x, y)
    override def complement = self
  }

  class Union[X, Y](rs: Seq[Relation[X, Y]]) extends Relation[X, Y] {
    def related(x: X, y: Y) = rs exists { r => r.related(x, y) }
    override def union[U <: X, V <: Y](that: Relation[U, V]) = new Union(that +: rs)
  }

  class Intersection[X, Y](rs: Seq[Relation[X, Y]]) extends Relation[X, Y] {
    def related(x: X, y: Y) = rs forall { r => r.related(x, y) }
    override def intersect[U <: X, V <: Y](that: Relation[U, V]) = new Intersection(that +: rs)
  }

  class Inverse[X, Y](val self: Relation[X, Y]) extends Relation[Y, X] {
    def related(x: Y, y: X) = self.related(y, x)
    override def inverse = self
  }

  class Product[X, Y, U, V](self: Relation[X, Y], that: Relation[U, V]) extends Relation[(X, U), (Y, V)] {
    def related(x: (X, U), y: (Y, V)) = self.related(x._1, y._1) && that.related(x._2, y._2)
  }

}