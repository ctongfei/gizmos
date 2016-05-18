package poly.collection

import poly.algebra.specgroup._
import poly.macroutil._

import scala.annotation.unchecked.{uncheckedVariance => uv}

/**
 * Represents an observer, which is a mechanism for receiving push-based streams.
 * Observers are the categorical dual of an [[Iterator]].
 *
 * @author Tongfei Chen
 * @since 0.1.0
 */
trait Observer[@sp(Int, Long, Double, Char) -T] { self =>

  def onNext(x: T): Unit

  def onError(error: Throwable): Unit

  def onCompleted(): Unit

  def write(x: Option[T]) = x match {
    case Some(v) => onNext(v)
    case None => onCompleted()
  }

  def writeFromArray(a: Array[T @uv], off: Int, len: Int): Unit = {
    FastLoop.ascending(off, off + len, 1) { i =>
      self.onNext(a(i))
    }
  }

  def writeFromArray(a: Array[T @uv]): Unit = writeFromArray(a, 0, a.length)

}
