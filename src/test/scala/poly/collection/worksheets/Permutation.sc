import poly.collection._

val a = Permutation(1, 2, 0)
a.inverse
a.reverse
a.pairs
a.reverse.pairs
a.inverse.pairs
a.bot
a.top
a.succ(1)
a.lt(1, 0)

a.compose(a.inverse)
Permutation.Group(4).elements foreach println


