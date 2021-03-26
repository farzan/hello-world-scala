object AbsValue {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "Absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-13))
    println(this formatAbs -13)
    println(AbsValue.formatBlock())
    println(this.formatBlock())
  }

  private def formatBlock() = {
    if (1 != 2)
      1.+(1)
    else
      {{{
        "x"
      }}}
  }
}
