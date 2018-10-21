package fintech.homework04

import scala.annotation.tailrec

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // реализовать функцию fold

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    @tailrec
    def foldFunc(value: Option[B], trees: List[Tree[A]])(f: A => B)(g: (B, B) => B): B = {
      if (trees.isEmpty)
        value.get
      else
        trees.head match {
          case leaf: Leaf[A] => foldFunc(
            value.map(value => g(value, f(leaf.value))) orElse Option(f(leaf.value)), trees.tail)(f)(g)
          case branch: Branch[A] => foldFunc(value, List(branch.left, branch.right) ++ trees.tail)(f)(g)
        }
    }
    foldFunc(None, List(t))(f)(g)
  }

  def size[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 1)((x: Int, y: Int) => x + y)

  def max(t: Tree[Int]): Int = fold[Int, Int](t)(x => x)((x, y) => math.max(x, y))

  def depth[A](t: Tree[A]): Int = fold[A, Int](t)(_ => 0)((x: Int, y: Int) => math.max(x, y) + 1)

  //здесь возможно придется прибегнуть к насильному указанию типа: Leaf(a): Tree[A]
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(x => Leaf(f(x)))((x, y) => new Branch[B](x, y))
}