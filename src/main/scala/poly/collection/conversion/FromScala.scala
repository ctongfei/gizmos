package poly.collection.conversion

import poly.algebra._
import poly.algebra.conversion.FromScala._
import poly.collection._
import poly.collection.mut._
import poly.collection.node._
import scala.{collection => sc}
import scala.collection.{mutable => scm}
import scala.language.implicitConversions

/**
 * Provides implicit conversions of [[scala.collection]] structures to Poly-collection ones.
 * @author Tongfei Chen
 * @since 0.1.0
 */
object FromScala {

  implicit class scalaTraversableAsPoly[T](xs: sc.Traversable[T]) extends AbstractTraversable[T] {
    def foreach[U](f: T => U) = xs foreach f
  }

  implicit class scalaIteratorAsPoly[T](xs: sc.Iterator[T]) extends Iterator[T] {
    var current: T = default[T]
    def advance() = {
      if (xs.hasNext) {
        current = xs.next()
        true
      }
      else false
    }
  }

  implicit class scalaIterableAsPoly[T](xs: sc.Iterable[T]) extends AbstractIterable[T] {
    def newIterator = xs.iterator
  }

  implicit class scalaLinearSeqAsPoly[T](xs: sc.LinearSeq[T]) extends AbstractSeq[T] {
    class WrappedNode(val s: sc.LinearSeq[T], override val isDummy: Boolean = false) extends SeqNode[T] {
      def data = s.head
      def next = {
        val t = s.tail
        new WrappedNode(t, t.isEmpty)
      }
    }
    override def newIterator = xs.iterator
    def headNode = new WrappedNode(xs, xs.isEmpty)
    override def apply(i: Int) = xs(i)
    override def length = xs.length
  }

  implicit class scalaIndexedSeqAsPoly[T](xs: sc.IndexedSeq[T]) extends AbstractIndexedSeq[T] {
    def fastApply(i: Int) = xs.apply(i)
    def fastLength = xs.length
  }

  implicit class scalaSetAsPoly[T](sset: sc.Set[T]) extends AbstractSet[T] {
    def eqOnKeys = Eq.default[T]
    def keys = sset
    def contains(x: T) = sset contains x
  }

  implicit class scalaMapAsPoly[K, V](smap: sc.Map[K, V]) extends AbstractMap[K, V] {
    def pairs = scalaIterableAsPoly(smap)
    def containsKey(x: K) = smap contains x
    def apply(k: K) = smap(k)
    def ?(k: K) = smap get k
    def eqOnKeys = Eq.default[K]
  }

  implicit class scalaSortedSetAsPoly[K](sset: sc.SortedSet[K]) extends AbstractSortedSet[K] {
    def keys = scalaIterableAsPoly(sset).asIfSorted(sset.ordering)
    def orderOnKeys = sset.ordering
    def contains(x: K) = sset contains x
  }

  implicit class scalaSortedMapAsPoly[K, V](smap: sc.SortedMap[K, V]) extends AbstractMap[K, V] with SortedMap[K, V] {
    def orderOnKeys = smap.ordering
    def pairs = scalaIterableAsPoly(smap).asIfSorted(smap.ordering contramap firstOfPair)
    def containsKey(x: K) = smap contains x
    def apply(k: K) = smap(k)
    def ?(k: K) = smap get k
  }

  implicit class scalaStackAsPoly[T](ss: scm.Stack[T]) extends Queue[T] {
    def elements = scalaIterableAsPoly(ss)
    def push(x: T) = ss push x
    def top = ss.top
    def pop() = ss.pop()
  }

  implicit class scalaArrayStackAsPoly[T](sas: scm.ArrayStack[T]) extends Queue[T] {
    def elements = scalaIterableAsPoly(sas)
    def push(x: T) = sas push x
    def top = sas.top
    def pop() = sas.pop()
  }

  implicit class scalaQueueAsPoly[T](sq: scm.Queue[T]) extends Queue[T] {
    def elements = scalaIterableAsPoly(sq)
    def push(x: T) = sq += x
    def top = sq.front
    def pop() = sq.dequeue()
  }

  implicit class scalaPriorityQueueAsPoly[T](spq: scm.PriorityQueue[T]) extends PriorityQueue[T] {
    def elements = scalaIterableAsPoly(spq)
    def orderOnElements = spq.ord
    def push(x: T) = spq += x
    def top = spq.head
    def pop() = spq.dequeue()
}

}
