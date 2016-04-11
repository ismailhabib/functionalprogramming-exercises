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
}

class Exercise6 {

}
