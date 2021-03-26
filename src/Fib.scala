import scala.annotation.tailrec

object Fib {
  def fib1(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else fib1(n - 1) + fib1(n - 2)
  }

  def fib2(n: Int): Int = {
    @tailrec
    def go(a: Int, b: Int, n: Int): Int = {
      if (n == 0) a
      else go(b, a + b, n - 1)
    }

    go(0, 1, n)
  }

  def main(args: Array[String]): Unit = {
    println(
      "fib1:",
      fib1(0),
      fib1(1),
      fib1(2),
      fib1(3),
      fib1(4),
      fib1(5),
      fib1(6),
      fib1(7),
      fib1(8),
    )

    println(
      "fib2:",
      fib2(0),
      fib2(1),
      fib2(2),
      fib2(3),
      fib2(4),
      fib2(5),
      fib2(6),
      fib2(7),
      fib2(8),
    )
  }
}
