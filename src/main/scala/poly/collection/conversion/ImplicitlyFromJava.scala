package poly.collection.conversion

import java.{lang => jl, util => ju, io => ji}
import java.util.{stream => jus}
import scala.language.implicitConversions

/**
 * Contains implicit conversions that converts Java collections ([[java.util]]) classes to Poly-collection classes.
 * @author Tongfei Chen
 * @since 0.1.0
 */
object ImplicitlyFromJava {

  implicit def javaIterableAsPoly[T](xs: jl.Iterable[T]) = new JavaIterableAsPoly(xs)
  implicit def javaIteratorAsPoly[T](xs: ju.Iterator[T]) = new JavaIteratorAsPoly(xs)
  implicit def javaIntIteratorAsPoly(xs: ju.PrimitiveIterator.OfInt) = new JavaIntIteratorAsPoly(xs)
  implicit def javaDoubleIteratorAsPoly(xs: ju.PrimitiveIterator.OfDouble) = new JavaDoubleIteratorAsPoly(xs)
  implicit def javaLongIteratorAsPoly(xs: ju.PrimitiveIterator.OfLong) = new JavaLongIteratorAsPoly(xs)

  implicit def javaStreamAsPoly[T](xs: jus.Stream[T]) = new JavaStreamAsPoly(xs)
  implicit def javaIntStreamAsPoly(xs: jus.IntStream) = new JavaIntStreamAsPoly(xs)
  implicit def javaLongStreamAsPoly(xs: jus.LongStream) = new JavaLongStreamAsPoly(xs)
  implicit def javaDoubleStreamAsPoly(xs: jus.DoubleStream) = new JavaDoubleStreamAsPoly(xs)
  
  implicit def javaListAsPoly[T](xs: ju.List[T]) = new JavaListAsPoly(xs)
  implicit def javaSetAsPoly[T](xs: ju.Set[T]) = new JavaSetAsPoly(xs)
  implicit def javaSortedSetAsPoly[T](xs: ju.SortedSet[T]) = new JavaSortedSetAsPoly(xs)
  implicit def javaMapAsPoly[K, V](xs: ju.Map[K, V]) = new JavaMapAsPoly(xs)
  implicit def javaSortedMapAsPoly[K, V](xs: ju.SortedMap[K, V]) = new JavaSortedMapAsPoly(xs)

  // implicit def javaNavigableSetAsPoly[T](xs: ju.NavigableSet[T]) extends AbstractSortedSet[T] with BidiSortedSet[T] {

  implicit def javaQueueAsPoly[T](xs: ju.Queue[T]) = new JavaQueueAsPoly(xs)

  // java.io

  implicit def javaInputStreamAsPoly(jis: ji.InputStream) = new JavaInputStreamAsPoly(jis)
  implicit def javaReaderAsPoly(jr: ji.Reader) = new JavaReaderAsPoly(jr)
  implicit def javaOutputStreamAsPoly(jos: ji.OutputStream) = new JavaOutputStreamAsPoly(jos)
  implicit def javaWriterAsPoly(jw: ji.Writer) = new JavaWriterAsPoly(jw)

}
