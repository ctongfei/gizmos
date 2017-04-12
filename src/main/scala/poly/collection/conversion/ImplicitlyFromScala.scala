package poly.collection.conversion

import scala.{collection => sc}
import scala.collection.{mutable => scm}
import scala.language.implicitConversions

/**
 * @author Tongfei Chen
 */
object ImplicitlyFromScala {

  implicit def scalaTraversableAsPoly[T](xs: sc.Traversable[T]) = new ScalaTraversableAsPoly(xs)
  implicit def scalaIterableAsPoly[T](xs: sc.Iterable[T]) = new ScalaIterableAsPoly(xs)
  implicit def scalaIteratorAsPoly[T](xs: sc.Iterator[T]) = new ScalaIteratorAsPoly(xs)
  implicit def scalaLinearSeqAsPoly[T](xs: sc.LinearSeq[T]) = new ScalaLinearSeqAsPoly(xs)
  implicit def scalaIndexedSeqAsPoly[T](xs: sc.IndexedSeq[T]) = new ScalaIndexedSeqAsPoly(xs)
  implicit def scalaSetAsPoly[T](xs: sc.Set[T]) = new ScalaSetAsPoly(xs)
  implicit def scalaSortedSetAsPoly[T](xs: sc.SortedSet[T]) = new ScalaSortedSetAsPoly(xs)
  implicit def scalaMapAsPoly[K, V](xs: sc.Map[K, V]) = new ScalaMapAsPoly(xs)
  implicit def scalaSortedMapAsPoly[K, V](xs: sc.SortedMap[K, V]) = new ScalaSortedMapAsPoly(xs)

  implicit def scalaStackAsPoly[T](ss: scm.Stack[T]) = new ScalaStackAsPoly(ss)
  implicit def scalaArrayStackAsPoly[T](ss: scm.ArrayStack[T]) = new ScalaArrayStackAsPoly(ss)
  implicit def scalaQueueAsPoly[T](sq: scm.Queue[T]) = new ScalaQueueAsPoly(sq)
  implicit def scalaPriorityQueueAsPoly[T](spq: scm.PriorityQueue[T]) = new ScalaPriorityQueueAsPoly(spq)

}
