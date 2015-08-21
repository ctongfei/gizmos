package poly.collection

import java.{util => ju}
import java.{lang => jl}
import scala.{collection => sc, _}
import scala.language.implicitConversions

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 * @since 0.1.0
 */
package object conversion {

  //region Scala/Java to Poly

  implicit def scalaTraversableAsPoly[T](xs: sc.Traversable[T]): Traversable[T] = new Traversable[T] {
    def foreach[U](f: T => U) = xs.foreach(f)
  }
  implicit def scalaIteratorAsPoly[T](xs: sc.Iterator[T]): Enumerator[T] = new Enumerator[T] {
    var current: T = default[T]
    def advance() = {
      if (xs.hasNext) {
        current = xs.next()
        true
      }
      else false
    }
  }
  implicit def scalaIterableAsPoly[T](xs: sc.Iterable[T]): Enumerable[T] = new Enumerable[T] {
    def newEnumerator = scalaIteratorAsPoly[T](xs.iterator)
  }
  implicit def scalaSeqAsPoly[T](xs: sc.Seq[T]): Seq[T] = new Seq[T] {
    override def newEnumerator = scalaIteratorAsPoly[T](xs.iterator)
    def headNode = ??? //TODO
    def apply(i: Int) = xs(i)
    def length = xs.length
  }


  implicit def javaIterableAsPoly[T](xs: jl.Iterable[T]): Enumerable[T] = new Enumerable[T] {
    def newEnumerator = javaIteratorAsPoly[T](xs.iterator())
  }
  implicit def javaIteratorAsPoly[T](xs: ju.Iterator[T]): Enumerator[T] = new Enumerator[T] {
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
    def length = xs.size
    def apply(i: Int) = xs.get(i)
    def update(i: Int, x: T) = xs.set(i, x)
  }
  implicit def javaMapAsPoly[K, V](jm: ju.Map[K, V]): Map[K, V] = new KeyMutableMap[K, V] {
    def add(x: K, y: V): Unit = jm.put(x, y)
    def clear(): Unit = jm.clear()
    def remove(x: K): Unit = jm.remove(x)
    def update(x: K, y: V): Unit = jm.put(x, y)
    def ?(x: K): Option[V] = Option(jm.get(x))
    def pairs: Enumerable[(K, V)] = jm.entrySet.map(e => e.getKey → e.getValue)
    def size = jm.size
    def apply(x: K): V = jm.get(x)
    def containsKey(x: K): Boolean = jm.containsKey(x)
  }
  //endregion

  //region Poly to Scala

  implicit class PolyEnumeratorAsScala[T](val e: Enumerator[T]) extends AnyVal {

    /**
     * Converts a Poly-collection enumerator to a Scala iterator.
     * @return An equivalent Scala iterator.
     */
    def asScalaIterator: sc.Iterator[T] = new sc.Iterator[T] {
      var nextElem: T = default[T]
      var nextElemFetched: Boolean = false

      def hasNext: Boolean = {
        if (nextElemFetched) true
        else {
          if (e.advance()) {
            nextElem = e.current
            nextElemFetched = true
            true
          }
          else false
        }
      }

      def next() = {
        if (nextElemFetched) {
          nextElemFetched = false
          nextElem
        }
        else {
          e.advance()
          e.current
        }
      }
    }
  }

  implicit class PolyTraversableAsScala[T](val xs: Traversable[T]) extends AnyVal {
    def asScalaTraversable: sc.Traversable[T] = new sc.Traversable[T] {
      def foreach[U](f: T => U): Unit = xs.foreach(f)
    }
  }

  implicit class PolyEnumerableAsScala[T](val xs: Enumerable[T]) extends AnyVal {
    def asScalaIterable: sc.Iterable[T] = new Iterable[T] {
      def iterator: Iterator[T] = xs.newEnumerator.asScalaIterator
    }
  }

  implicit class PolySeqAsScala[T](val xs: Seq[T]) extends AnyVal {
    def asScalaSeq: sc.Seq[T] = new sc.Seq[T] {
      def length: Int = xs.length
      def apply(i: Int): T = xs(i)
      def iterator: Iterator[T] = xs.newEnumerator.asScalaIterator
    }
  }

  implicit class PolyIndexedSeqAsScala[T](val xs: IndexedSeq[T]) extends AnyVal {
    def asScalaIndexedSeq: sc.IndexedSeq[T] = new sc.IndexedSeq[T] {
      def length: Int = xs.length
      def apply(i: Int): T = xs(i)
    }
  }

  implicit class PolyMapAsScala[K, V](val xs: Map[K, V]) extends AnyVal {
    def asScalaMap: sc.Map[K, V] = new sc.DefaultMap[K, V] {
      def get(key: K): Option[V] = xs ? key
      def iterator: Iterator[(K, V)] = xs.pairs.newEnumerator.asScalaIterator
    }
  }
  //endregion

}
