package multiplication

/**
 * Created by mario on 10-10-16.
 */
object IntegerMultiplication {

  def multiply(x: String, y: String): Int = {
    val a = if (x.length < y.length) y.toList.map(_.asDigit).reverse else x.toList.map(_.asDigit).reverse
    val b = if (x.length < y.length) x.toList.map(_.asDigit).reverse else y.toList.map(_.asDigit).reverse
    var total = 0
    for {i <- 0 to b.length-1} {
      var carry = 0
      var sum = List[Int]()
      for {j <- 0 to a.length-1} {
        val p = b(i)*a(j)+carry
        sum = p.toString.takeRight(1).toInt +: sum
        if (p > 10) carry = p.toString.take(1).toInt else carry = 0
      }
      sum = carry +: sum
      total += sum.mkString.toInt*Math.pow(10, i).toInt
    }
    total
  }

}
