package poly

import scala.language.implicitConversions

package object collection {

  /** Returns the first element of a pair. */
  @inline def first[A](pair: (A, _)) = pair._1

  /** Returns the second element of a pair. */
  @inline def second[B](pair: (_, B)) = pair._2

  @inline private[poly] def default[T]: T = {
    class Default {
      var default: T = _
    }
    (new Default).default
  }
  
  private[poly] def nextPowerOfTwo(x: Int): Int = {
    var c = x - 1
    c |= c >>> 1
    c |= c >>> 2
    c |= c >>> 4
    c |= c >>> 8
    c |= c >>> 16
    c + 1
  }

  implicit def arrayAsIndexedSeq[T](a: Array[T]): IndexedSeq[T] = new ArrayAsIndexedSeq[T](a)

  implicit def stringAsIndexedSeq(s: String): IndexedSeq[Char] = new StringAsIndexedSeq(s)

}
