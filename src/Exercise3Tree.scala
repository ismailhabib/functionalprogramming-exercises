/**
  * Created by imu on 3/7/2016.
  */

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](tree: Tree[A]): Int = tree match {
  case Leaf(_) => 1
  case Branch(left, right) => 2 + size(left) + size(right)
}

def maximum(tree: Tree[Int]): Int = tree match {
  case Leaf(value) => value
  case Branch(left, right) => maximum(left).max(maximum(right))
}

def depth[A](tree: Tree[A]): Int = tree match {
  case Leaf(_) => 1
  case Branch(left, right) => (1 + depth(left)).max(1 + (depth(right)))
}

def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
  case Leaf(value) => Leaf(f(value))
  case Branch(left, right) => Branch(map(left)(f), map(right)(f))
}

object Exercise3Tree {

}
