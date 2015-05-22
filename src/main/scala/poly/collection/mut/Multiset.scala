package poly.collection.mut

import poly.collection.factory._
import poly.collection.impl._
import poly.collection._

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class Multiset[T, C[+T1] <: Set[T1]] private() {

  var inner: C[(T, Int)] = _

}

