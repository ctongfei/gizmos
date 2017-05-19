package poly.collection.impl

import poly.collection._
import poly.collection.typeclass._

/**
 * Adapted from the public-domain code from
 * [[https://github.com/aappleby/smhasher/blob/master/src/MurmurHash3.cpp]].
 * @author Austin Appleby
 * @author Tongfei Chen
 */
object MurmurHash3 {

  @inline final def mix(hash: Int, data: Int): Int = {
    var h = mixLast(hash, data)
    h = Integer.rotateLeft(h, 13)
    h * 5 + 0xe6546b64
  }

  @inline final def mixLast(hash: Int, data: Int): Int = {
    var k = data
    k *= 0xcc9e2d51
    k = Integer.rotateLeft(k, 15)
    k *= 0x1b873593
    hash ^ k
  }

  final def finalizeHash(hash: Int, length: Int): Int = fmix32(hash ^ length)

  @inline final def fmix32(_h: Int): Int = {
    var h = _h
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16
    h
  }

  final def symmetricHash[T](xs: Traversable[T], seed: Int = 0xb592f7ae)(implicit T: Hashing[T]) = {
    var n = 0
    var sum = 0
    var xor = 0
    var prod = 1
    for (x <- xs) {
      val h = T hash x
      sum += h
      xor ^= h
      if (h != 0) prod *= h
      n += 1
    }
    var h = seed
    h = mix(h, sum)
    h = mix(h, xor)
    h = mixLast(h, prod)
    finalizeHash(h, n)
  }

  final def sequentialHash[T](xs: Traversable[T], seed: Int = 0xe73a8b15)(implicit T: Hashing[T]) = {
    var n = 0
    var h = seed
    for (x <- xs) {
      h = mix(h, T hash x)
      n += 1
    }
    finalizeHash(h, n)
  }

}
