
import scala.annotation.tailrec

object HOF {
  def main(args: Array[String]): Unit = {
    println(formatFunc("Abstract", -45, abs))
    println(formatFunc("Factorial", 5, factorial))
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0)
        acc
      else
        go(n - 1, n * acc)

    go(n, 1)
  }

  private def formatFunc(title: String, x: Int, func: Int => Int) = {
    val msg = "%s of %d is %d"
    msg.format(title, x, func(x))
  }
}
