package poly.collection.mut

import poly.collection._
import poly.collection.builder._
import poly.collection.exception._
import poly.collection.factory._
import poly.collection.impl._
import poly.util.specgroup._
import scala.reflect._

/**
 * An array-backed stack.
 *
 * @author Tongfei Chen
 */
class ArrayStack[T] private(private var data: ResizableSeq[T] = null) extends Queue[T] {

  def elements = data

  override def size = data.fastLength

  def push(x: T): Unit = data.appendInplace(x)

  override def pushAll(xs: Traversable[T]) = {
    xs.reverse foreach push
  }

  def top: T = {
    if (isEmpty) throw new QueueEmptyException
    data(data.fastLength - 1)
  }

  def pop(): T = {
    val x = top
    data.deleteAt(data.fastLength - 1)
    x
  }

}

object ArrayStack extends Factory[ArrayStack] {

  implicit def newBuilder[T]: Builder[T, ArrayStack[T]] = new Builder[T, ArrayStack[T]] {
    var data = new ResizableSeq[T]()
    def sizeHint(n: Int) = data.ensureCapacity(n)
    def add(x: T) = data.appendInplace(x)
    def result = new ArrayStack(data)
  }

}
