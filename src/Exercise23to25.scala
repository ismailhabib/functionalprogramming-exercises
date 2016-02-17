/**
  * Created by imu on 2/16/2016.
  */
object Exercise23to25 {

  case class MyA(myVal: Int)

  case class MyB(myVal: Int)

  case class MyC(myVal: Int)

  def main(args: Array[String]): Unit = {
    println(curry((a: MyA, b: MyB) => MyC(a.myVal + b.myVal))(MyA(1))(MyB(2)))

    println(uncurry((a: MyA) => (b: MyB) => MyC(a.myVal + b.myVal))(MyA(1), MyB(2)))

    println(compose((b: MyB) => MyC(b.myVal + 1), (a: MyA) => MyB(a.myVal + 1))(MyA(1)))
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    return (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    return (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    return (a: A) => f(g(a))
  }
}
