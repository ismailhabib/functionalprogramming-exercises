package laziness

import Stream._
import scala.Some

/**
  * Created by imu on 3/23/2016.
  */

sealed trait Stream[+A] {

  //Exercise 5.1
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  //Exercise 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().take(n - 1)
    case _ => this
  }

  //Exercise 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  //Exercise 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  //Exercise 5.5
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  //Exercise 5.6
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  //Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, acc) => cons(f(h), acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, acc) =>
      if (f(h)) cons(h, acc)
      else acc)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, acc) => cons(h, acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, acc) => f(h).append(acc))

  //Exercise 5.13

  def map2[B](f: A => B): Stream[B] = unfold(this)(x => x match {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  })

  def take2(n: Int): Stream[A] = unfold((this, n))(x => x match {
    case (Cons(h, t), 1) => Some((h(), (empty, 0)))
    case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
    case _ => None
  })

  def takeWhileWithUnfold(p: A => Boolean): Stream[A] = unfold(this)(x => x match {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  })

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_, _))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    Stream.unfold((this, s2)) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) ->(t(), empty[B]))
      case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    }

  //Exercise 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).takeWhile(!_._2.isEmpty) forAll {
      case (h, h2) => h == h2
    }

  //Exercise 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some((s, s.drop(1)))
    } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails.exists(_ startsWith s)

  //Exercise 5.16
  /*
  The function can't be implemented using `unfold`, since `unfold` generates elements of the `Stream` from left to right. It can be implemented using `foldRight` though.
  The implementation is just a `foldRight` that keeps the accumulated value and the stream of intermediate results, which we `cons` onto during each iteration. When writing folds, it's common to have more state in the fold than is needed to compute the result. Here, we simply extract the accumulated list once finished.
  */
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  //Exercise 5.8
  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  //Exercise 5.9
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  //Exercise 5.10
  def fibs(): Stream[Int] = {
    def rec(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, rec(b, b + a))
    }
    rec(0, 1)
  }

  //Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  //Exercise 5.12
  def fibs2(): Stream[Int] = unfold((0, 1))(x => x match {
    case (a, b) => Some(a, (b, a + b))
  })

  def constant2[A](a: A): Stream[A] = unfold(a)(x => Some((x, x)))

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def ones2(): Stream[Int] = unfold(1)(x => Some(1, 1))

}


object Exercise5 extends App {
  val strm = Stream(1, 2, 5, 1)
  println(strm.takeWhile2(_ < 3).toList)
  println(strm.headOption)
  println(Stream.constant(5).take(10).toList)
  println(Stream.from(5).take(10).toList)
  println(Stream.fibs.take(10).toList)

  println(Stream.ones2.take(10).toList)
  println(Stream.constant2(5).take(10).toList)
  println(Stream.from2(5).take(10).toList)
  println(Stream.fibs2.take(10).toList)
}
