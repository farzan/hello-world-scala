import scala.annotation.tailrec

object FindFirst {
  def main(args: Array[String]): Unit = {
    val ints = Array(1, 2, 3, 4, 5)

    println("===")
    println(findFirst(ints, (n: Int) => n == 3))
    println(findFirst(ints, (n: Int) => n == 5))
    println(findFirst(ints, (n: Int) => n == 6))

    println("---")
    val strings = Array("One", "Two", "Three")
    println(findFirst(strings, (s: String) => s == "Two"))
    println(findFirst(strings, (s: String) => s == "Four"))

    println("===")

    val intsOrdered = Array(1, 3, 4, 8, 9)
    val intsNotOrdered = Array(1, 3, 4, 9, 8)
    def f(a: Int, b: Int) = a < b
    println(isSorted(intsOrdered, f))
    println(isSorted(intsNotOrdered, f))
  }

  def findFirst [A](ar: Array[A], f: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int =
      if (n > ar.length - 1) -1
      else if (f(ar(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted [A](ar: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean =
      if (n > ar.length - 2) true
      else if (!ordered(ar(n), ar(n + 1))) false
      else loop(n + 1)

    loop(0)
  }
}
