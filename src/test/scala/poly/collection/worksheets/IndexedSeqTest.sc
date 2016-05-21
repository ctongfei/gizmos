import poly.collection._
import poly.collection.mut._
import poly.algebra._
import poly.algebra.syntax._

val a = ArraySeq(1,1,2,2,4,4,2,2,0,0,-1,-1, -2,-2,-2)
a.sortInplace()(Order by { _ % 3})
a

val b = ArraySeq(0, 1, 2)
val c = (b cartesianProduct b).asMap.map{case (x, y) => x + y}
val d = c.curried
d(0)

