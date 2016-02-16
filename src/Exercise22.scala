import scala.reflect.ClassTag

/**
  * Created by imu on 2/4/2016.
  */
object Exercise22 {
  def main(args: Array[String]): Unit = {
    println(isSorted(Array(0, 1, 5, 6, 7, 8), (a: Int, b: Int) => a <= b))
    println(isSorted(Array(0, 1, 5, 6, 7, 8, 7), (a: Int, b: Int) => a <= b))
    println(isSorted2(Array(0, 1, 5, 6, 7, 8), (a: Int, b: Int) => a <= b))
    println(isSorted2(Array(0, 1, 5, 6, 7, 8, 7), (a: Int, b: Int) => a <= b))
    println(isSorted3(List(0, 1, 5, 6, 7, 8), (a: Int, b: Int) => a <= b))
    println(isSorted3(List(0, 1, 5, 6, 7, 8, 7), (a: Int, b: Int) => a <= b))
    println(isOrdered(List(0, 1, 2)))
    println(isOrdered(List(2, 1, 2)))

  }

  @annotation.tailrec
  def isSortedBest[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2) true
    else if (ordered(as.head, as.tail.head)) isSortedBest(as.tail, ordered)
    else false
  }

  @annotation.tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length == 0 || as.length == 1)
      true
    else if (ordered(as.head, as.tail.head))
      isSorted(as.tail, ordered)
    else
      false
  }

  @annotation.tailrec
  def isSorted2[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = as match {
    case as if as.length == 0 || as.length == 1 => true
    case as if ordered(as.head, as.tail.head) => isSorted2(as.tail, ordered)
    case _ => false
  }

  //using list
  @annotation.tailrec
  def isSorted3[A](as: List[A], ordered: (A, A) => Boolean): Boolean = as match {
    case Nil | _ :: Nil => true
    case a :: b :: rest if ordered(a, b) => isSorted3(b :: rest, ordered)
    case _ => false
  }

  //using array while keeping pattern matching pretty
  @annotation.tailrec
  def isSorted4[A: ClassTag](as: Array[A], ordered: (A, A) => Boolean): Boolean = as match {
    case Array() | Array(_) => true
    case Array(a, b, rest@_*) if ordered(a, b) => isSorted4(b +: rest.toArray, ordered)
    case _ => false
  }

  def isOrdered[T](xs: Seq[T])(implicit ordering: Ordering[T]): Boolean = xs.zip(xs.tail).forall { case (x, y) => ordering.lteq(x, y) }

  // O(2n)
  def isOrderedLazy[T](xs: Seq[T])(implicit ordering: Ordering[T]): Boolean = isOrdered(xs.toStream) // O(n)

}
