package fintech.homework04

import org.scalatest.{Fact, FlatSpec, Matchers}

import scala.annotation.tailrec

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

  "Map of Tree" should "return tree with the same structure" in {
    val newTree = Tree.map(tree)(x => x.toString)
    var correct_structure = true
    def rec(left: Tree[Int], right: Tree[Int], newLeft: Tree[String], newRight: Tree[String]): Unit = {
      left match {
        case leaf: Leaf[Int] =>
          if (leaf.value.toString != newLeft.asInstanceOf[Leaf[String]].value)
            correct_structure = false
        case branch: Branch[Int] => rec(branch.left, branch.right, newLeft.asInstanceOf[Branch[String]].left,
          newLeft.asInstanceOf[Branch[String]].right)
      }
      right match {
        case leaf: Leaf[Int] =>
          if (leaf.value.toString != newRight.asInstanceOf[Leaf[String]].value)
            correct_structure = false
        case branch: Branch[Int] => rec(branch.left, branch.right, newLeft.asInstanceOf[Branch[String]].left,
          newLeft.asInstanceOf[Branch[String]].right)
      }
    }
    rec(tree.left, tree.right, newTree.asInstanceOf[Branch[String]].left,
      newTree.asInstanceOf[Branch[String]].right)
    correct_structure should be (true)
  }
}