package poly.collection.impl.trie

import poly.algebra._
import poly.algebra.syntax._
import poly.collection._

/**
 * An implementation of a compressed prefix trie.
 * @author Tongfei Chen
 * @since 0.1.0
 */
class PrefixTrie[C: Order, V] extends SortedMap[Seq[C], V] {
  def orderOnKeys = Seq.LexicographicOrder[C]
  def pairs = ???
  def containsKey(x: Seq[C]) = ???
  def apply(k: Seq[C]) = ???
  def ?(k: Seq[C]) = ???
}
