/**
  * Created by imu on 2/24/2016.
  */

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))


  def tail[A](list: List[A]) = list match {
    case Nil => Nil
    case Cons(x, tail) => tail
  }

  def setHead[A](head: A, list: List[A]): List[A] = list match {
    case Nil => Cons(head, Nil)
    case Cons(head, tail) => Cons(head, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => tail(l)
    case _ => drop(tail(l), n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, tail) => {
      if (f(x))
        dropWhile(tail, f)
      else
        l
    }
  }

  def dropWhile2[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, tail) if f(x) =>
      dropWhile(tail, f)
    case Cons(x, tail) => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, tail) => Cons(x, init(tail))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int = foldRight(as, 0)((x, y) => 1 + y)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def rec(as: List[A], acc: B): B = as match {
      case Nil => acc
      case Cons(x, xs) => rec(xs, f(acc, x))
    }
    rec(as, z)
  }

  def sum(ints: List[Int]): Int = foldLeft(ints, 0)((x, y) => x + y)

  def product(ds: List[Double]): Double = foldLeft(ds, 1.0)((x, y) => x * y)

  def lengthLeft[A](as: List[A]): Int = foldLeft(as, 0)((x, y) => x + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((x, y) => Cons(y, x))

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((a, b) => f(b, a))

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x, y) => Cons(x, y))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((x, y) => append(x, y))
}

object Exercise3 {

  def main(args: Array[String]) = {
    println(List.init(Cons(1, Cons(2, Nil))))
    println(List.init(Cons(1, Cons(2, Cons(3, Nil)))))
    println(List.length(Cons(1, Cons(2, Cons(3, Nil)))))
    println(List.sum(Cons(1, Cons(2, Cons(3, Nil)))))
    println(List.product(Cons(1, Cons(2, Cons(3, Nil)))))
    println(List.lengthLeft(Cons(1, Cons(2, Cons(3, Nil)))))
    println(List.append(Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Nil))))
  }
}