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

    println("7===")
    println(List.sum2(List(1, 2, 3, 4)))
    println(List.product2(List(1, 2, 3, 4)))

    println("8===")
    println(List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)))
    println(List.foldRight(List(1.0, 2, 3), Nil: List[Double])(Cons(_, _)))
    println(List.length(List(1, 2, 3, 4, 5)))
    println(List.length2(List(1, 2, 3, 4, 5)))

    println("9===")
    println(List.foldRight(List(1, 2, 3, 4), 1.0)(_ * _))
    println(List.foldLeft(List(1, 2, 3, 4), 1.0)(_ * _))
    println(List.foldLeft(List(1, 2, 3, 4), 0)(_ + _))

    println("10===")
    println(List.reverse(List(1, 2, 3)))
    println(List.reverse(List(1)))
    println(List.reverse(Nil))

    println(List.reverse2(List(1, 2, 3)))
    println(List.reverse2(List(1)))
    println(List.reverse2(Nil))

    println("11===")
    println(List.append2(List(1, 2, 3), List(7, 8, 9)))
    println(List.append3(List(1, 2, 3), List(7, 8, 9)))
    println(List.concat(List(List(1, 2), List(3, 4), List(5, 6))))

    println("3.16 ==")
    println(List.addOne(List(1, 2, 3)))
    println(List.addOne(Nil))
    println(List.addOne2(List(1, 2, 3)))
    println(List.addOne2(Nil))

    println("3.17 ==")
    println(List.doubleToString(List(1, 2, 3)))

    println("3.18 ==")
    println(List.map(List(1, 2, 3))(a => a + 1))

    println("3.19 ==")
    println(List.filter(List(1, 2, 3))(a => a != 2))
    println(List.filter(List(1, 2, 3))(a => a == 4))

    println("3.20 ==")
    println(List.flatMap(List(1,2,3))(i => List(i,i)))

    println("3.21 ==")
    println(List.filterViaFlatMap(List(1, 2, 3))(a => a != 2))
    println(List.filterViaFlatMap(List(1, 2, 3))(a => a == 4))

    println("=== 3.22")
    println(List.addPairwise(List(1, 2, 3), List(4, 5, 6)))
    println(List.addPairwise(List(1, 2), List(4, 5, 6)))
    println(List.addPairwise(List(1, 2, 3), List(4, 5)))

    println("=== 3.22")
    println(List.zipWith(List(1, 2, 3), List(1.1, 2.2, 3.3))((a, b) => String.format("{%d, %f}", a, b)))


  }
}
