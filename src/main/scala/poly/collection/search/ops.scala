package poly.collection.search

import poly.algebra._
import poly.collection._

/**
 * Enables searching methods on objects.
 * @author Tongfei Chen
 * @since 0.1.0
 */
object ops extends LowerPriorityImplicits {

  implicit class withSearchOps[T](val x: T) extends AnyVal {
    /**
     * Performs depth first tree traversal given the transition function.
     */
    def depthFirstTreeTraversal[U >: T](f: U => Traversable[U]) =
      StateSpace(f).depthFirstTreeTraversal(x)

    /**
     * Performs breadth first tree traversal given the transition function.
     */
    def breadthFirstTreeTraversal[U >: T](f: U => Traversable[U]) =
      StateSpace(f).breadthFirstTreeTraversal(x)

    /**
     * Performs depth first graph traversal given the transition function.
     */
    def depthFirstTraversal[U >: T : Eq](f: U => Traversable[U]) =
      StateSpaceWithEq(f).depthFirstTraversal(x)

    /**
     * Performs breadth first graph traversal given the transition function.
     */
    def breadthFirstTraversal[U >: T : Eq](f: U => Traversable[U]) =
      StateSpaceWithEq(f).breadthFirstTraversal(x)

    /**
     * Performs uniform cost traversal given the transition function.
     */
    def uniformCostTraversal[U >: T : Eq, R : OrderedAdditiveGroup](f: U => Traversable[(U, R)]) =
      WeightedStateSpace(f).uniformCostTraversal(x)
  }

  implicit class withWeightedStateOps[S, C](val s: S)(implicit S: WeightedStateSpace[S, C]) {
    def succ = S.succ(s)
    def succWithCost = S.succWithCost(s)
  }

  implicit class withNodeOps[N, S](val n: N)(implicit I: SearchNodeInfo[N, S]) {
    def state: S = I.state(n)
    def next(s: S): N = I.nextNode(n)(s)
  }

  implicit class withWeightedNodeOps[N, S, C](val n: N)(implicit I: WeightedSearchNodeInfo[N, S, C]) {
    def state = I.state(n)
    def next(s: S, c: C): N = I.nextNode(n)(s, c)
  }

}

trait LowerPriorityImplicits {
  implicit class withStateOps[S](val s: S)(implicit S: StateSpace[S]) {
    def succ = S.succ(s)
  }

}