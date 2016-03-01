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
    case Nil => throw new NoSuchElementException("List is empty")
    case Cons(x, tail) => tail
  }

  def setHead[A](head: A, list: List[A]): List[A] = list match {
    case Nil => Cons(head, Nil)
    case Cons(head2, tail) => Cons(head, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case n if n < 0 => throw new NoSuchElementException("n is too large")
    case 0 => l
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

  //  def foldLeft3[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(l, (x:B) => x)((x,acc)=>y=>acc(f(y,x))))(z)

  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x, y) => Cons(x, y))

  //Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])((x, y) => append(x, y))

  //Exercise 3.16
  def addOne(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, addOne(xs))
  }

  //Exercise 3.17
  def toString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, toString(xs))
  }

  //Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  //Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
    case Cons(x, xs) => filter(xs)(f)
  }

  def filterOdd(as: List[Int]): List[Int] = filter(as)(x => x % 2 == 0)

  //Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  //Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else Nil)

  def filterOdd2(as: List[Int]): List[Int] = filter2(as)(x => x % 2 == 0)

  //Exercise 3.22
  def addList(ints: List[Int], ints2: List[Int]): List[Int] = ints match {
    case Nil => Nil
    case Cons(x, xs) => ints2 match {
      case Nil => Nil
      case Cons(y, ys) => Cons(x + y, addList(xs, ys))
    }
  }

  //Exercise 3.23
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = l1 match {
    case Nil => Nil
    case Cons(x, xs) => l2 match {
      case Nil => Nil
      case Cons(y, ys) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
  }

  //Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def rec(sup: List[A], pSub:List[A]): Boolean = {
      sup match {
        case Nil => pSub match {
          case Nil => true
          case Cons(y, ys) => false
        }
        case Cons(x, xs) => pSub match {
          case Nil => true
          case Cons(y, ys) if (x == y) => rec(xs, ys) || hasSubsequence(xs, sub)
          case Cons(y, ys) => rec(xs, sub)
        }
      }
    }
    rec(sup, sub)
  }


}

object Exercise3 {

  def main(args: Array[String]) = {
    println(List.drop(List(1, 2, 3, 4), 2))
    println(List.init(Cons(1, Cons(2, Nil))))
    println(List.init(Cons(1, Cons(2, Cons(3, Nil)))))
    println(List.length(Cons(1, Cons(2, Cons(3, Nil)))))
    println(List.sum(Cons(1, Cons(2, Cons(3, Nil)))))
    println(List.product(Cons(1, Cons(2, Cons(3, Nil)))))
    println(List.lengthLeft(Cons(1, Cons(2, Cons(3, Nil)))))
    println(List.append(Cons(1, Cons(2, Cons(3, Nil))), Cons(4, Cons(5, Nil))))
    println(List.concat(Cons(Cons(4, Cons(5, Nil)), Cons(Cons(4, Cons(5, Nil)), Cons(Cons(4, Cons(5, Nil)), Nil)))))
    println("Add one: " + List.addOne(List(1, 2, 3, 4, 5)))
    println("To string: " + List.toString(List(1, 2, 3, 4, 5)))
    println("Map x=x+1: " + List.map(List(1, 2, 3))(x => x + 1))
    println("Filter odd: " + List.filterOdd(List(8, 3, 2, 5, 7, 4, 4)))
    println("Flat Map x,x+1: " + List.flatMap(List(1, 2, 3))(x => List(x, x + 1)))
    println("Filter odd 2: " + List.filterOdd2(List(8, 3, 2, 5, 7, 4, 4)))
    println("Add 2 list: " + List.addList(List(5, 4, 3), List(5, 10, 15)))
    println("Zip with: " + List.zipWith(List(5, 4, 3), List(5, 10, 15))((x, y) => x + y))

    val list1 = List(1, 2, 3, 4, 5)
    val list2 = List(1, 2)
    val list3 = List(3, 4)
    val list4 = List(5, 6)
    val list5 = List(1, 3)
    val list6 = List(2)

    println(List.hasSubsequence(list1, list1))
    println(List.hasSubsequence(list1, list2))
    println(List.hasSubsequence(list1, list3))
    println(List.hasSubsequence(list1, list4))
    println(List.hasSubsequence(list1, list5))
    println(List.hasSubsequence(list1, list6))
  }
}