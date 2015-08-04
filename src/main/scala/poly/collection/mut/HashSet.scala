package poly.collection.mut

import poly.algebra._
import poly.collection._
import poly.collection.conversion._
import poly.collection.impl._


/**
 * @author Tongfei Chen (ctongfei@gmail.com).
 */
class HashSet[T] private(val data: java.util.HashSet[T]) extends MutableSet[T] {

  def add(x: T) = data.add(x)

  def remove(x: T) = data.remove(x)

  def contains(x: T) = data.contains(x)

  def elements = data

  def size: Int = data.size
}

