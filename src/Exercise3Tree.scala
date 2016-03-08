/**
  * Created by imu on 3/7/2016.
  */

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  //Exercise 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 2 + size(left) + size(right)
  }

  //Exercise 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  //Exercise 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => (1 + depth(left)).max(1 + (depth(right)))
  }

  //Exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  //Exercise 3.29
  def fold[A, B](tree: Tree[A])(g: A => B)(f: (B, B) => B): B = {
    def recc(tree: Tree[A]): B =
      tree match {
        case Leaf(value) => g(value)
        case Branch(left, right) => f(recc(left), recc(right))
      }

    recc(tree)
  }

  def size2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)

  def maximum2(tree: Tree[Int]): Int = fold(tree)(value => value)((x, y) => x.max(y))

  def depth2[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((x, y) => x.max(y))

  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(value => Leaf(f(value)): Tree[B])((l, r) => Branch(l, r))
}

object Exercise3Tree extends App {
  val myTree = Branch(Branch(Leaf(3), Leaf(4)), Leaf(1))
  println("Size: " + Tree.size2(myTree))

}
