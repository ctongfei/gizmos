import poly.algebra._
import poly.algebra.syntax._
import poly.collection._
import poly.collection.mut._

val a = HashSet(1, 2, 3, 4, 5, 6, 7)

val q = a quotient (Eq by {_ % 3})

q.elements

q.contains(HashSet())