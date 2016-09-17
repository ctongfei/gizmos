package poly.collection.node

/**
 * @author Tongfei Chen
 */
trait BidiTreeNodeLike[+T, +N <: BidiTreeNodeLike[T, N]] extends TreeNodeLike[T, N] with BidiNodeLike[T, N] with NodeWithParentLike[T, N] { self: N => }

trait BidiTreeNode[+T] extends BidiTreeNodeLike[T, BidiTreeNode[T]] with TreeNode[T] with BidiNode[T] with NodeWithParent[T]
