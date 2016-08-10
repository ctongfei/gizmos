import poly.collection._

val t = Tree("S",
  Tree("NP",
    Tree("Pizzas")
  ),
  Tree("VP",
    Tree("are"),
    Tree("horrible")
  )
)
t.preOrder
t.postOrder
t.levelOrder
