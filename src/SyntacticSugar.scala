object SyntacticSugar {
  def main(args: Array[String]): Unit = {
    def lessThan(a: Int, b: Int): Boolean =
      if (a < b) true
      else false

    println(lessThan(1, 2))
//    println(lessThan.apply(1, 2))

    val lessThan2 = new Function2[Int, Int, Boolean] {
      def apply(a: Int, b: Int): Boolean = a < b
    }

    println(lessThan2(1, 2))
    println(lessThan2.apply(1, 2))
  }
}
