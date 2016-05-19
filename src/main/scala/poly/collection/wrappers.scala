package poly.collection

import poly.collection.mut._
import poly.collection.builder._
import scala.language.implicitConversions

private[poly] trait ImplicitWrappers {
  implicit def arrayAsPoly[T](a: Array[T]): IndexedSeq[T] = new ArrayAsIndexedSeq[T](a)
  implicit def stringAsPoly(s: String): IndexedSeq[Char] = new StringAsIndexedSeq(s)
  implicit def booleanFunctionAsPoly[T](f: T => Boolean): Predicate[T] = new BooleanFunctionAsPredicate[T](f)
  implicit def stringBuilderAsPoly(sb: StringBuilder): Builder[Char, String] = new StringBuilderAsBuilder(sb)
  implicit def javaStringBuilderAsPoly(sb: java.lang.StringBuilder): Builder[Char, String] = new JavaStringBuilderAsBuilder(sb)
}

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
  override def sizeHint(n: Int) = underlying.sizeHint(n)
}

private[collection]
class JavaStringBuilderAsBuilder(val underlying: java.lang.StringBuilder) extends Builder[Char, String] {
  def addInplace(x: Char) = underlying.append(x)
  def result = underlying.toString
}