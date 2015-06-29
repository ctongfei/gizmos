package poly

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

  type =?>[-X, +Y] = PartialFunction[X, Y]

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
