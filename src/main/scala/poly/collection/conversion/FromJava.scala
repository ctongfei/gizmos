package poly.collection.conversion

import java.{lang => jl, util => ju, io => ji}
import java.util.{stream => jus}
import poly.algebra._
import poly.algebra.conversion.FromJava._
import poly.collection._
import poly.collection.mut._
import scala.language.implicitConversions

/**
 * Contains implicit conversions that converts Java collections ([[java.util]]) classes to Poly-collection classes.
 * @author Tongfei Chen
 * @since 0.1.0
 */
object FromJava {

  implicit class javaIterableAsPoly[T](xs: jl.Iterable[T]) extends AbstractIterable[T] {
    def newIterator = javaIteratorAsPoly[T](xs.iterator())
  }

  implicit class javaIteratorAsPoly[T](xs: ju.Iterator[T]) extends Iterator[T] {
    private[this] var curr: T = default[T]
    def advance() = {
      if (xs.hasNext) {
        curr = xs.next()
        true
      }
      else false
    }
    def current = curr
  }

  implicit class javaIntIteratorAsPoly(xs: ju.PrimitiveIterator.OfInt) extends Iterator[Int] {
    private[this] var curr: Int = 0
    def advance() = {
      if (xs.hasNext) {
        curr = xs.nextInt()
        true
      }
      else false
    }
    def current = curr
  }

  implicit class javaDoubleIteratorAsPoly(xs: ju.PrimitiveIterator.OfDouble) extends Iterator[Double] {
    private[this] var curr: Double = 0.0
    def advance() = {
      if (xs.hasNext) {
        curr = xs.nextDouble()
        true
      }
      else false
    }
    def current = curr
  }

  implicit class javaLongIteratorAsPoly(xs: ju.PrimitiveIterator.OfLong) extends Iterator[Long] {
    private[this] var curr: Long = 0l
    def advance() = {
      if (xs.hasNext) {
        curr = xs.nextLong()
        true
      }
      else false
    }
    def current = curr
  }

  implicit class javaStreamAsPoly[T](xs: jus.Stream[T]) extends AbstractIterable[T] {
    def newIterator = xs.iterator()
  }

  implicit class javaIntStreamAsPoly(xs: jus.IntStream) extends AbstractIterable[Int] {
    def newIterator = xs.iterator()
  }

  implicit class javaLongStreamAsPoly(xs: jus.LongStream) extends AbstractIterable[Long] {
    def newIterator = xs.iterator()
  }

  implicit class javaDoubleStreamAsPoly(xs: jus.DoubleStream) extends AbstractIterable[Double] {
    def newIterator = xs.iterator()
  }
  
  implicit class javaListAsPoly[T](xs: ju.List[T]) extends ValueMutableIndexedSeq[T] {
    def fastLength = xs.size
    def fastApply(i: Int) = xs.get(i)
    def update(i: Int, x: T) = xs.set(i, x)
  }

  implicit class javaSetAsPoly[T](xs: ju.Set[T]) extends KeyMutableSet[T] {
    def removeInplace(x: T) = xs.remove(x)
    def addInplace(x: T) = xs.add(x)
    def keyEq = Eq.default[T]
    def contains(x: T) = xs.contains(x)
    override def size = xs.size()
    def keys = Iterable.ofIterator(xs.iterator())
    def clear() = xs.clear()
  }

  implicit class javaSortedSetAsPoly[T](xs: ju.SortedSet[T]) extends AbstractSortedSet[T] {
    def keys = new SortedIterable[T] {
      implicit def elementOrder = xs.comparator()
      def newIterator = xs.iterator()
    }
    def contains(x: T) = xs.contains(x)
    def keyOrder = xs.comparator()
  }

  implicit class javaNavigableSetAsPoly[T](xs: ju.NavigableSet[T]) extends AbstractSortedSet[T] with BidiSortedSet[T] {
    def keys: BidiSortedIterable[T] = new BidiSortedIterable[T] {
      implicit def elementOrder = xs.comparator()
      def newReverseIterator = xs.descendingIterator()
      def newIterator = xs.iterator()
    }
    def keyOrder = xs.comparator()
    def contains(x: T) = xs.contains(x)
  }

  implicit class javaQueueAsPoly[T](xs: ju.Queue[T]) extends Queue[T] {
    def push(x: T) = xs.add(x)
    def top = xs.peek()
    def pop() = xs.remove()
    def elements = xs
  }

  /*
  implicit class javaDequeAsPoly[T](xs: ju.Deque[T]) extends Deque[T] {
    def bottom = xs.peekLast()
    def popTop() = xs.removeFirst()
    def popBottom() = xs.removeLast()
    def push(x: T) = xs.add(x)
    xs.add()
    def top = xs.peekFirst()
    def elements = xs
  }
  */

  implicit class javaMapAsPoly[K, V](jm: ju.Map[K, V]) extends AbstractMap[K, V] with KeyMutableMap[K, V] {
    def keySet = jm.keySet
    def addInplace(x: K, y: V) = jm.put(x, y)
    def clear() = jm.clear()
    def removeInplace(x: K) = jm.remove(x)
    def update(x: K, y: V) = jm.put(x, y)
    def ?(x: K) = Option(jm.get(x))
    override def size = jm.size
    def apply(x: K) = jm.get(x)
  }

  // java.io

  implicit class javaInputStreamAsPoly(jis: ji.InputStream) extends Iterator[Byte] {
    private[this] var c: Int = 0
    def advance() = {
      if (c != -1) {
        c = jis.read()
        true
      } else false
    }
    def current = c.toByte
    override def readToArray(a: Array[Byte], off: Int, len: Int) = jis.read(a, off, len)
  }

  implicit class javaReaderAsPoly(jr: ji.Reader) extends Iterator[Char] {
    private[this] var c: Int = 0
    def advance() = {
      if (c != -1) {
        c = jr.read()
        true
      } else false
    }
    def current = c.toChar
    override def readToArray(a: Array[Char], off: Int, len: Int) = jr.read(a, off, len)
  }

  /*
  implicit class javaOutputStreamAsPoly(jos: ji.OutputStream) extends Observer[Byte] {
    def onCompleted() = jos.close()
    def onError(error: Throwable) = throw error
    def onNext(x: Byte) = jos.write(x)
    override def writeFromArray(a: Array[Byte], off: Int, len: Int) = jos.write(a, off, len)
  }

  implicit class javaWriterAsPoly(jw: ji.Writer) extends Observer[Char] {
    def onCompleted() = jw.close()
    def onError(error: Throwable) = throw error
    def onNext(x: Char) = jw.write(x)
    override def writeFromArray(a: Array[Char], off: Int, len: Int) = jw.write(a, off, len)
  }
  */

}
