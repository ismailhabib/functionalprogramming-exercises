package purefunctionalstate

/**
  * Created by imu on 04-Apr-16.
  */
trait RNG {
  def nextInt: (Int, RNG)
}


case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  //Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nb, newRNG) = rng.nextInt
    if (nb < 0) (-nb + 1, newRNG) else (nb, newRNG)
  }

  //Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (nb, newNRG) = rng.nextInt
    (nb.toDouble / (Int.MaxValue.toDouble + 1), newNRG)
  }

  //Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nb, newNRG) = rng.nextInt
    val (nb2, newNRG2) = RNG.double(newNRG)
    ((nb, nb2), newNRG2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (nb, newNRG) = RNG.double(rng)
    val (nb2, newNRG2) = newNRG.nextInt
    ((nb, nb2), newNRG2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (nb, newRNG) = RNG.double(rng)
    val (nb2, newRNG2) = RNG.double(newRNG)
    val (nb3, newRNG3) = RNG.double(newRNG2)
    ((nb, nb2, nb3), newRNG3)
  }

  //Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List(), rng)
    else {
      val (x, r1) = rng.nextInt
      val (xs, r2) = ints(count - 1)(r1)
      (x :: xs, r2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  //Exercise 6.5
  def doubleWithMap = map(nonNegativeInt)(x => x / (Int.MaxValue.toDouble + 1))

  //Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  //Exercise 6.7
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((x, acc) => map2(x, acc)((a,b) => a::b))

  def sequence_2[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs match {
      case Nil => unit(Nil)
      case h :: t => map2(h, sequence_2(t))(_ :: _)
    }

  def ints2(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  //Exercise 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  //Exercise 6.9
  def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))



}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
}

class Exercise6 {

}
