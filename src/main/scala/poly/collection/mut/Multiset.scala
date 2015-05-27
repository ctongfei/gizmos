package poly.collection.mut

import poly.collection.factory._
import poly.collection.impl._
import poly.collection._
import scala.language.higherKinds

/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class Multiset[T, C[T1] <: Set[T1]] private() {

  var inner: C[(T, Int)] = _

}

class Multimap[T, C[T1] <: Set[T1], I[T2] <: Set[T2]] private() {



}