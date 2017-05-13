package poly.collection.conversion

import poly.collection._
import poly.collection.mut._
import scala.reflect._

/**
 * @author Tongfei Chen
 */
class ArrayAsPoly[T](val underlying: Array[T]) extends ValueMutableIndexedSeq[T] {
  def fastLength = underlying.length
  def fastApply(i: Int) = underlying(i)
  def update(i: Int, x: T) = underlying(i) = x
}

class JavaCharSequenceAsPoly(val underlying: java.lang.CharSequence) extends IndexedSeq[Char] {
  def fastLength = underlying.length
  def fastApply(i: Int) = underlying.charAt(i)
  override def slice(i: Int, j: Int) = underlying.subSequence(i, j)
}

class JavaStringBuilderAsPoly(val underlying: java.lang.StringBuilder) extends Builder[Char, String] {
  def add(x: Char) = underlying.append(x)
  def result = underlying.toString
  override def sizeHint(n: Int) = underlying.ensureCapacity(n)
}

class JavaIteratorAsPoly[T](xs: java.util.Iterator[T]) extends AbstractIterator[T] {
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

class JavaIterableAsPoly[T](xs: java.lang.Iterable[T]) extends AbstractIterable[T] {
  def newIterator = new JavaIteratorAsPoly[T](xs.iterator())
}

class JavaIntIteratorAsPoly(xs: java.util.PrimitiveIterator.OfInt) extends AbstractIterator[Int] {
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

class JavaDoubleIteratorAsPoly(xs: java.util.PrimitiveIterator.OfDouble) extends AbstractIterator[Double] {
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

class JavaLongIteratorAsPoly(xs: java.util.PrimitiveIterator.OfLong) extends AbstractIterator[Long] {
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

class JavaStreamAsPoly[T](xs: java.util.stream.Stream[T]) extends AbstractIterable[T] {
  def newIterator = new JavaIteratorAsPoly(xs.iterator())
}

class JavaIntStreamAsPoly(xs: java.util.stream.IntStream) extends AbstractIterable[Int] {
  def newIterator = new JavaIntIteratorAsPoly(xs.iterator())
}

class JavaDoubleStreamAsPoly(xs: java.util.stream.DoubleStream) extends AbstractIterable[Double] {
  def newIterator = new JavaDoubleIteratorAsPoly(xs.iterator())
}

class JavaLongStreamAsPoly(xs: java.util.stream.LongStream) extends AbstractIterable[Long] {
  def newIterator = new JavaLongIteratorAsPoly(xs.iterator())
}

class JavaListAsPoly[T](xs: java.util.List[T]) extends AbstractIndexedSeq[T] with ValueMutableIndexedSeq[T] {
  def fastLength = xs.size
  def fastApply(i: Int) = xs.get(i)
  def update(i: Int, x: T) = xs.set(i, x)
}

class JavaMapAsPoly[K, V](xs: java.util.Map[K, V]) extends AbstractMap[K, V] with KeyMutableMap[K, V] {
  def keySet = new JavaSetAsPoly(xs.keySet)
  def add_!(x: K, y: V) = xs.put(x, y)
  def clear_!() = xs.clear()
  def remove_!(x: K) = xs.remove(x)
  def update(x: K, y: V) = xs.put(x, y)
  def ?(x: K) = Option(xs.get(x))
  override def size = xs.size
  def apply(x: K) = xs.get(x)
  override def pairs = Iterable.ofIterator(new JavaIteratorAsPoly(xs.entrySet().iterator())).map(e => e.getKey -> e.getValue)
}

class JavaSetAsPoly[T](xs: java.util.Set[T]) extends AbstractSet[T] with KeyMutableSet[T] {
  def remove_!(x: T) = xs.remove(x)
  def add_!(x: T) = xs.add(x)
  def keyEq = Hashing.default[T]
  def contains(x: T) = xs.contains(x)
  override def size = xs.size()
  def keys = Iterable.ofIterator(new JavaIteratorAsPoly(xs.iterator()))
  def clear_!() = xs.clear()
}

class JavaSortedSetAsPoly[T](xs: java.util.SortedSet[T]) extends AbstractSortedSet[T] {
  def keys: SortedIterable[T] = new SortedIterable[T] {
    implicit def elementOrder = xs.comparator()
    def newIterator = new JavaIteratorAsPoly(xs.iterator())
  }
  def contains(x: T) = xs.contains(x)
  def keyOrder = xs.comparator()
}

class JavaSortedMapAsPoly[K, V](xs: java.util.SortedMap[K, V]) extends AbstractKeySortedMap[K, V] with KeyMutableMap[K, V] {
  def keySet: SortedSet[K] = new SortedSet[K] {
    def keyOrder = xs.comparator()
    def keys = new SortedIterable[K] {
      def elementOrder: Order[K] = xs.comparator()
      def newIterator = new JavaIteratorAsPoly(xs.keySet().iterator())
    }
    def contains(x: K) = xs.containsKey(x)
  }
  def ?(k: K) = Option(xs.get(k))
  def apply(k: K) = xs.get(k)
  override def pairs = new SortedIterable[(K, V)] {
    def elementOrder: Order[(K, V)] = (xs.comparator(): Order[K]) on first
    def newIterator = Iterable.ofIterator(new JavaIteratorAsPoly(xs.entrySet().iterator())).map(e => e.getKey -> e.getValue).newIterator
  }
  def add_!(k: K, v: V) = xs.put(k, v)
  def remove_!(k: K) = xs.remove(k)
  def clear_!() = xs.clear()
  def update(k: K, v: V) = xs.put(k, v)
}

//TODO: j.u.NavigableSet

//TODO: j.u.Deque

class JavaQueueAsPoly[T](xs: java.util.Queue[T]) extends Queue[T] {
  def enqueue(x: T) = xs.add(x)
  def front = xs.peek()
  def dequeue() = xs.remove()
  def elements = new JavaIterableAsPoly(xs)
}

class JavaInputStreamAsPoly(jis: java.io.InputStream) extends AbstractIterator[Byte] {
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

class JavaReaderAsPoly(jr: java.io.Reader) extends AbstractIterator[Char] {
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

class JavaOutputStreamAsPoly(jos: java.io.OutputStream) extends Builder[Byte, Unit] {
  def add(x: Byte) = jos.write(x)
  def result = jos.close()
  override def writeFromArray(a: Array[Byte], off: Int, len: Int) = jos.write(a, off, len)
}

class JavaWriterAsPoly(jw: java.io.Writer) extends Builder[Char, Unit] {
  def add(x: Char) = jw.write(x)
  def result = jw.close()
  override def writeFromArray(a: Array[Char], off: Int, len: Int) = jw.write(a, off, len)
}
