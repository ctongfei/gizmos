package poly

import scala.language.implicitConversions

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
package object collection {

  @inline def default[T]: T = {
    class Default {
      var default: T = _
    }
    (new Default).default
  }

  /**
   * Returns the first element of a pair.
   */
  @inline def first[A, B](pair: (A, B)) = pair._1

  /**
   * Returns the second element of a pair.
   */
  @inline def second[A, B](pair: (A, B)) = pair._2
  
  private[collection] def nextPowerOfTwo(x: Int): Int = {
    var c = x - 1
    c |= c >>> 1
    c |= c >>> 2
    c |= c >>> 4
    c |= c >>> 8
    c |= c >>> 16
    c + 1
  }

}
