package poly.collection.conversion

import scala.{collection => sc}
import scala.collection.{mutable => scm}
/**
 * @author Tongfei Chen
 */
object FromScala {

  implicit class scalaTraversableAsPoly[T](val xs: sc.Traversable[T]) extends AnyVal {
    def asPoly = new ScalaTraversableAsPoly(xs)
  }
  implicit class scalaIteratorAsPoly[T](val xs: sc.Iterator[T]) extends AnyVal {
    def asPoly = new ScalaIteratorAsPoly(xs)
  }
  implicit class scalaIterableAsPoly[T](val xs: sc.Iterable[T]) extends AnyVal {
    def asPoly = new ScalaIterableAsPoly(xs)
  }
  implicit class scalaLinearSeqAsPoly[T](val xs: sc.LinearSeq[T]) extends AnyVal {
    def asPoly = new ScalaLinearSeqAsPoly(xs)
  }
  implicit class scalaIndexedSeqAsPoly[T](val xs: sc.IndexedSeq[T]) extends AnyVal {
    def asPoly = new ScalaIndexedSeqAsPoly(xs)
  }
  implicit class scalaSetAsPoly[T](val xs: sc.Set[T]) extends AnyVal {
    def asPoly = new ScalaSetAsPoly(xs)
  }
  implicit class scalaSortedSetAsPoly[T](val xs: sc.SortedSet[T]) extends AnyVal {
    def asPoly = new ScalaSortedSetAsPoly(xs)
  }
  implicit class scalaMapAsPoly[K, V](val xs: sc.Map[K, V]) extends AnyVal {
    def asPoly = new ScalaMapAsPoly(xs)
  }
  implicit class scalaSortedMapAsPoly[K, V](val xs: sc.SortedMap[K, V]) extends AnyVal {
    def asPoly = new ScalaSortedMapAsPoly(xs)
  }
  implicit class scalaStackAsPoly[T](val xs: scm.Stack[T]) extends AnyVal {
    def asPoly = new ScalaStackAsPoly(xs)
  }
  implicit class scalaArrayStackAsPoly[T](val xs: scm.ArrayStack[T]) extends AnyVal {
    def asPoly = new ScalaArrayStackAsPoly(xs)
  }
  implicit class scalaQueueAsPoly[T](val xs: scm.Queue[T]) extends AnyVal {
    def asPoly = new ScalaQueueAsPoly(xs)
  }
  implicit class scalaPriorityQueueAsPoly[T](val xs: scm.PriorityQueue[T]) extends AnyVal {
    def asPoly = new ScalaPriorityQueueAsPoly(xs)
  }
}
