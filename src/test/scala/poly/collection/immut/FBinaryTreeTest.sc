import poly.collection.immut.FBinaryTree._

val t = Cons(1, Cons(2, Cons(3), Cons(4)), Cons(5))

t.fold(0)((x, y, z) => x + y + z)
