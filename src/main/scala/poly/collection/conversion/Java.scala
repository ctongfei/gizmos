package poly.collection.conversion

import java.{lang => jl, util => ju}
import poly.algebra._
import poly.collection._
import scala.language.implicitConversions

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
object Java {

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

  implicit def javaListAsPoly[T](xs: ju.List[T]): IndexedSeq[T] = new DataMutableIndexedSeq[T] {
    def fastLength = xs.size
    def fastApply(i: Int) = xs.get(i)
    def update(i: Int, x: T) = xs.set(i, x)
  }

  implicit def javaMapAsPoly[K, V](jm: ju.Map[K, V]): Map[K, V] = new KeyMutableMap[K, V] {
    def equivOnKey = Equiv.default[K]
    def add(x: K, y: V): Unit = jm.put(x, y)
    def clear(): Unit = jm.clear()
    def remove(x: K): Unit = jm.remove(x)
    def update(x: K, y: V): Unit = jm.put(x, y)
    def ?(x: K): Option[V] = Option(jm.get(x))
    def pairs: Iterable[(K, V)] = jm.entrySet.map(e => e.getKey → e.getValue)
    def size = jm.size
    def apply(x: K): V = jm.get(x)
    def containsKey(x: K): Boolean = jm.containsKey(x)
  }


}
