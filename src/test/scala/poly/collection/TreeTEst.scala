package poly.collection

/**
 * @author Tongfei Chen
 */
object TreeTest extends App {


  val t = Tree("S",
    Tree("NP",
      Tree("Pizzas")
    ),
    Tree("VP",
      Tree("are"),
      Tree("horrible")
    )
  )
  for (x <- t.postOrder) println(x)

}
