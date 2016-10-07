package week1

/**
 * Created by mcastro on 10/7/2016.
 */
object MatrixMultiplication {

  def multiply(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    val c = Array.fill[Array[Int]](a.length)(Array.fill[Int](b(0).length)(0))
    for {k <- 0 to a.length-1} {
      for {i <- 0 to a.length-1} {
        for {j <- 0 to b.length-1} {
          c(k)(i) += a(k)(j)*b(j)(i)
        }
      }
    }
    c
  }

}
