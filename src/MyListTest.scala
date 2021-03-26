import farzan.hello.list._

object MyListTest {
  def main(args: Array[String]): Unit = {
    val i1: List[Int] = Nil
    val i2: List[Int] = Cons(1, Nil)
    val i3: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

    println(i1)
    println(i2)
    println(i3)

    println("1===")
    println(List.sum(i1))
    println(List.sum(i2))
    println(List.sum(i3))

    println("2===")
    val d3: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    println(d3)
    println(List.product(d3))

    println("3===")
    println(List(1, 2, 3) match { case _ => 42 })
    println(List(1, 2, 3) match { case Cons(h, _) => h })
    println(List(1, 2, 3) match { case Cons(_, t) => t })

    println("4===")
    println(
      List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + List.sum(t)
        case _ => 101
      }
    )

    println("5===")
    println(List.tail(List(1, 2, 3)))
    println(List.setHead(List(1, 2, 3), 9))
    println(List.drop(List(1, 2, 3, 4, 5), 3))
    println(List.dropWhile(List(1, 2, 3, 4, 5), (n: Int) => n < 3))

    println("6===")
    println(List.append(List(1, 2, 3), List(7, 8, 9)))
    println(List.init(List(1, 2, 3, 4)))
    println(List.dropWhile2(List(1, 2, 3, 4, 5))(n => n < 4))
  }
}
