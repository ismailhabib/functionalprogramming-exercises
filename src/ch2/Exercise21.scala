package ch2

/**
  * Created by imu on 2/4/2016.
  */

object Exercise21 {
  def main(args: Array[String]): Unit = {
    for (i <- 0 to 12) {
      println(fibonacci(i))
      println(fibonacci2(i))
    }
  }

  def fibonacci(n: Int): Int = n match {
    case 0 => 0
    case 1 => 1
    case _ => fibonacci(n - 2) + fibonacci(n - 1)
  }

  def fibonacci2(n: Int): Int = {
    @annotation.tailrec
    def fibonacciRec(a: Int, b: Int, n: Int): Int = n match {
      case 0 => a
      case _ => fibonacciRec(b, a + b, n - 1)
    }

    fibonacciRec(0, 1, n)
  }
}
