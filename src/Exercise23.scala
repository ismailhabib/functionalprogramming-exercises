/**
  * Created by imu on 2/16/2016.
  */
object Exercise23 {

  case class MyA(myVal: Int)

  case class MyB(myVal: Int)

  case class MyC(myVal: Int)

  def main(args: Array[String]): Unit = {
    val myCurry = curry((a: MyA, b: MyB) => MyC(a.myVal + b.myVal))
    val myCurry2 = myCurry(MyA(1))
    println(myCurry2(MyB(5)))

  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    return (a: A) => (b: B) => f(a,b)
  }
}
