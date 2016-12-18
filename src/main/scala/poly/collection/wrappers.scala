package poly.collection

import poly.collection.mut._

import scala.language.implicitConversions
import scala.reflect._

class ScalaStringBuilderAsBuilder(val underlying: StringBuilder) extends Builder[Char, String] {
  def add(x: Char) = underlying.append(x)
  def result = underlying.result()
  override def sizeHint(n: Int) = underlying.ensureCapacity(n)
}

class BooleanFunctionAsPredicate[T](val underlying: T => Boolean) extends Predicate[T] {
  def apply(x: T) = underlying(x)
}
