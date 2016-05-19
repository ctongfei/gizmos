package poly.collection.search

/**
 * @author Tongfei Chen
 */
object ops extends LowerPriorityImplicits {

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