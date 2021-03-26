import scala.annotation.tailrec

object Factorial {
  def factorial1(n: Int): Int = {
    if (n <= 0)
      1
    else
      n * factorial1(n - 1)
  }

  def factorial2(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0)
        acc
      else
        go(n - 1, n * acc)

    go(n, 1)
  }

  def main(args: Array[String]): Unit = {
    println(factorial1(4))
    println(factorial2(4))
  }
}
