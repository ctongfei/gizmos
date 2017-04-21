package poly.collection.conversion

import java.{lang => jl, util => ju, io => ji}
import java.util.{stream => jus}
/**
 * @author Tongfei Chen
 */
object FromJava {

  implicit class javaIterableAsPoly[T](val xs: jl.Iterable[T]) extends AnyVal {
    def asPoly = new JavaIterableAsPoly(xs)
  }
  implicit class javaIteratorAsPoly[T](val xs: ju.Iterator[T]) extends AnyVal {
    def asPoly = new JavaIteratorAsPoly(xs)
  }
  implicit class javaIntIteratorAsPoly(val xs: ju.PrimitiveIterator.OfInt) extends AnyVal {
    def asPoly = new JavaIntIteratorAsPoly(xs)
  }
  implicit class javaDoubleIteratorAsPoly(val xs: ju.PrimitiveIterator.OfDouble) extends AnyVal {
    def asPoly = new JavaDoubleIteratorAsPoly(xs)
  }
  implicit class javaLongIteratorAsPoly(val xs: ju.PrimitiveIterator.OfLong) extends AnyVal {
    def asPoly = new JavaLongIteratorAsPoly(xs)
  }
  implicit class javaStreamAsPoly[T](val xs: jus.Stream[T]) extends AnyVal {
    def asPoly = new JavaStreamAsPoly(xs)
  }
  implicit class javaIntStreamAsPoly(val xs: jus.IntStream) extends AnyVal {
    def asPoly = new JavaIntStreamAsPoly(xs)
  }
  implicit class javaLongStreamAsPoly(val xs: jus.LongStream) extends AnyVal {
    def asPoly = new JavaLongStreamAsPoly(xs)
  }
  implicit class javaDoubleStreamAsPoly(val xs: jus.DoubleStream) extends AnyVal {
    def asPoly = new JavaDoubleStreamAsPoly(xs)
  }
  implicit class javaListAsPoly[T](val xs: ju.List[T]) extends AnyVal {
    def asPoly = new JavaListAsPoly(xs)
  }
  implicit class javaSetAsPoly[T](val xs: ju.Set[T]) extends AnyVal {
    def asPoly = new JavaSetAsPoly(xs)
  }
  implicit class javaSortedSetAsPoly[T](val xs: ju.SortedSet[T]) extends AnyVal {
    def asPoly = new JavaSortedSetAsPoly(xs)
  }
  implicit class javaQueueAsPoly[T](val xs: ju.Queue[T]) extends AnyVal {
    def asPoly = new JavaQueueAsPoly(xs)
  }
  implicit class javaMapAsPoly[K, V](val xs: ju.Map[K, V]) extends AnyVal {
    def asPoly = new JavaMapAsPoly(xs)
  }
  implicit class javaSortedMapAsPoly[K, V](val xs: ju.SortedMap[K, V]) extends AnyVal {
    def asPoly = new JavaSortedMapAsPoly(xs)
  }

  implicit class javaInputStreamAsPoly(val jis: ji.InputStream) extends AnyVal {
    def asPoly = new JavaInputStreamAsPoly(jis)
  }
  implicit class javaReaderAsPoly(val jr: ji.Reader) extends AnyVal {
    def asPoly = new JavaReaderAsPoly(jr)
  }
  implicit class javaOutputStreamAsPoly(val jos: ji.OutputStream) extends AnyVal {
    def asPoly = new JavaOutputStreamAsPoly(jos)
  }
  implicit class javaWriterAsPoly(val jw: ji.Writer) extends AnyVal {
    def asPoly = new JavaWriterAsPoly(jw)
  }
  
}
