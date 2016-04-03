package poly.collection

import poly.collection.mut._
import poly.collection.builder._

private[collection]
class ArrayAsIndexedSeq[T](val underlying: Array[T]) extends ValueMutableIndexedSeq[T] {
  def fastLength = underlying.length
  def fastApply(i: Int) = underlying(i)
  def update(i: Int, x: T) = underlying(i) = x
}

private[collection]
class StringAsIndexedSeq(val underlying: String) extends IndexedSeq[Char] {
  def fastLength = underlying.length
  def fastApply(i: Int) = underlying.charAt(i)
}

private[collection]
class BooleanFunctionAsPredicate[T](val underlying: T => Boolean) extends Predicate[T] {
  def apply(x: T) = underlying(x)
}

private[collection]
class StringBuilderAsBuilder(val underlying: StringBuilder) extends Builder[Char, String] {
  def addInplace(x: Char) = underlying.append(x)
  def result = underlying.result()
  def sizeHint(n: Int) = underlying.sizeHint(n)
}

private[collection]
class JavaStringBuilderAsBuilder(val underlying: java.lang.StringBuilder) extends Builder[Char, String] {
  def addInplace(x: Char) = underlying.append(x)
  def result = underlying.toString
  def sizeHint(n: Int) = {}
}