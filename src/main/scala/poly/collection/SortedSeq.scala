package poly.collection

import poly.collection.node._

/**
 * @author Tongfei Chen
 */
trait SortedSeq[T] extends Seq[T] with SortedIterable[T] { self =>

  override def filter(f: T => Boolean): SortedSeq[T] = {
    class FilteredSeqNode(val node: SeqNode[T]) extends SeqNode[T] {
      override def isDummy = node.isDummy
      def data = node.data
      def next = {
        var nextNode = node.next
        while (nextNode.notDummy && !f(nextNode.data)) nextNode = nextNode.next
        new FilteredSeqNode(nextNode)
      }
    }
    Seq.ofDummyNode(new FilteredSeqNode(dummy)).asIfSorted(orderOnElements)
  }

  override def filterNot(f: T => Boolean) = filter(x => !f(x))

  //TODO: thenSortBy
}
