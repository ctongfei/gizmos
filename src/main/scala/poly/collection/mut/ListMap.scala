package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.exception._
import poly.collection.impl._

/**
 * @author Tongfei Chen
 */
class ListMap[K, V] private(private val data: SinglyLinkedList[KeyValuePair[K, V]])(implicit val equivOnKey: Equiv[K]) extends KeyMutableMap[K, V] {

  def size = data.size

  private[this] def locateKey(x: K): (data.Node, data.Node) = {
    var p = data.dummy
    var c = data.dummy.next
    while (c ne data.dummy) {
      if (x == c.data.key) return (p, c)
      p = c
      c = c.next
    }
    null
  }

  def containsKey(x: K) = locateKey(x) ne null

  def ?(x: K) = {
    val pc = locateKey(x)
    if (pc eq null) None
    else {
      val (_, c) = pc
      Some(c.data.value)
    }
  }

  def add(x: K, y: V) = {
    val pc = locateKey(x)
    if (pc eq null) data.prependInplace(KeyValuePair(x, y))
    else {
      val (_, c) = pc
      c.data.value = y
    }
  }

  def clear() = data.clear()

  def remove(x: K) = {
    val pc = locateKey(x)
    if (pc ne null) {
      val (p, c) = pc
      p.next = c.next
    }
  }

  def update(x: K, y: V) = {
    val pc = locateKey(x)
    if (pc eq null) throw new KeyNotFoundException(x)
    val (_, c) = pc
    c.data.value = y
  }


  def apply(x: K): V = locateKey(x)._2.data.value

  def pairs = data.map(_.toTuple)


}

//TODO:!!! change to MapFactory
object ListMap {
  def apply[K, V](xs: (K, V)*): ListMap[K, V] = {
    val l = new SinglyLinkedList[KeyValuePair[K, V]]
    for (x ← xs) l.appendInplace(KeyValuePair(x._1, x._2))
    new ListMap[K, V](l)
  }
}