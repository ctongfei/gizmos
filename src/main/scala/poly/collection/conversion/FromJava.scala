package poly.collection.conversion

import java.{lang => jl, util => ju}
import poly.algebra._
import poly.algebra.conversion.FromJava._
import poly.collection._
import poly.collection.mut._
import scala.language.implicitConversions

/**
 * @author Tongfei Chen
 */
object FromJava {

  implicit def javaIterableAsPoly[T](xs: jl.Iterable[T]): Iterable[T] = new AbstractIterable[T] {
    def newIterator = javaIteratorAsPoly[T](xs.iterator())
  }

  implicit def javaIteratorAsPoly[T](xs: ju.Iterator[T]): Iterator[T] = new Iterator[T] {
    var current: T = default[T]
    def advance() = {
      if (xs.hasNext) {
        current = xs.next()
        true
      }
      else false
    }
  }

  implicit def javaListAsPoly[T](xs: ju.List[T]): IndexedSeq[T] = new ValueMutableIndexedSeq[T] {
    def fastLength = xs.size
    def fastApply(i: Int) = xs.get(i)
    def update(i: Int, x: T) = xs.set(i, x)
  }

  implicit def javaSetAsPoly[T](xs: ju.Set[T]): Set[T] = new KeyMutableSet[T] {
    def remove(x: T) = xs.remove(x)
    def add(x: T) = xs.add(x)
    def equivOnKey = Equiv.default[T]
    def contains(x: T) = xs.contains(x)
    override def size = xs.size()
    def keys = Iterable.ofIterator(xs.iterator())
    def clear() = xs.clear()
  }

  implicit def javaSortedSetAsPoly[T](xs: ju.SortedSet[T]): SortedSet[T] = new SortedSet[T] {
    def keys = new SortedIterable[T] {
      implicit def order = xs.comparator()
      def newIterator = xs.iterator()
    }
    def contains(x: T) = xs.contains(x)
  }

  implicit def javaQueueAsPoly[T](xs: ju.Queue[T]): Queue[T] = new Queue[T] {
    def push(x: T) = xs.add(x)
    def top = xs.peek()
    def pop() = xs.remove()
    def elements = xs
  }

  implicit def javaDequeAsPoly[T](xs: ju.Deque[T]): Deque[T] = new Deque[T] {
    def bottom = xs.peekLast()
    def popTop() = xs.removeFirst()
    def popBottom() = xs.removeLast()
    def push(x: T) = xs.add(x)
    def top = xs.peekFirst()
    def elements = xs
  }

  implicit def javaMapAsPoly[K, V](jm: ju.Map[K, V]): Map[K, V] = new KeyMutableMap[K, V] {
    def equivOnKey = Equiv.default[K]
    def add(x: K, y: V): Unit = jm.put(x, y)
    def clear(): Unit = jm.clear()
    def remove(x: K): Unit = jm.remove(x)
    def update(x: K, y: V): Unit = jm.put(x, y)
    def ?(x: K): Option[V] = Option(jm.get(x))
    def pairs: Iterable[(K, V)] = jm.entrySet().elements.map(e => e.getKey → e.getValue)
    override def size = jm.size
    def apply(x: K): V = jm.get(x)
    def containsKey(x: K): Boolean = jm.containsKey(x)
  }




}
