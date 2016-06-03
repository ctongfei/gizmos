package poly.collection.impl.trie

/**
 * @author Tongfei Chen
 */
trait TrieNodeLike[A, +N <: TrieNodeLike[A, N]] { self: N =>

}
