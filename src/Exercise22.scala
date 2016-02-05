/**
  * Created by imu on 2/4/2016.
  */
object Exercise22 {
  def main(args: Array[String]): Unit = {
    println(isSorted(Array(0, 1, 5, 6, 7, 8), (a: Int, b: Int) => a <= b))
    println(isSorted(Array(0, 1, 5, 6, 7, 8, 7), (a: Int, b: Int) => a <= b))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def isSortedRec(as: Array[A]): Boolean = {
      if (as.length == 0 || as.length == 1)
        true
      else if (ordered(as.head, as.tail.head))
        isSortedRec(as.tail)
      else
        false
    }

    isSortedRec(as)
  }
}
