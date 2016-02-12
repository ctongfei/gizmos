import poly.collection.mut._
import poly.algebra._
import poly.algebra.ops._
import poly.algebra.implicits._

val a = ArraySeq(1,1,2,2,4,4,2,2,0,0,-1,-1, -2,-2,-2)
a.sortInplace()(WeakOrder by { _ % 3})
a

