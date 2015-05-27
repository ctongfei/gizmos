package poly.collection.mut

import poly.collection._
import poly.collection.impl._


/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
abstract class HashSet[T] private(val data: HashTable[T]) extends MutSet[T] {

  def add(x: T) = data.insert(x)

  def remove(x: T) = data.remove(x)

  def contains(x: T) = data.locate(x) != -1

}
