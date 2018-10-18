package fintech.homework04

import org.scalatest.{FlatSpec, Matchers}

class TreeSpec extends FlatSpec with Matchers {

  val tree = new Branch[Int](
    new Branch[Int](new Leaf[Int](1), new Leaf[Int](2)),
    new Leaf[Int](3)
  )

  val otherSizeTree = new Branch[Int](new Branch[Int](
    new Branch[Int](new Leaf[Int](1), new Leaf[Int](2)),
    new Branch[Int](new Leaf[Int](3), new Leaf[Int](4))),
    new Leaf[Int](5)
  )

  "Size of Tree" should "be equal to number of nodes" in {
    Tree.size(tree) should be (3)
    Tree.size(otherSizeTree) should be (5)
  }

  "Max value of Tree" should "be equal to maximum value of leaves" in {
    Tree.max(tree) should be (3)
    Tree.max(otherSizeTree) should be (5)
  }

  "Depth of Tree" should "be equal to maximum value of length of branches" in {
    Tree.depth(tree) should be (2)
    Tree.depth(otherSizeTree) should be (4)
  }

  "Map of Tree" should "return tree with the same options" in {
    val newTree = Tree.map(tree)(x => x.toString)
    Tree.size(newTree) should be (Tree.size(tree))
    Tree.depth(newTree) should be (Tree.depth(tree))
  }

}