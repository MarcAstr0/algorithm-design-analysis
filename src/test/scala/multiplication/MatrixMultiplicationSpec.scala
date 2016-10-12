package multiplication

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by mcastro on 10/7/2016.
 */
class MatrixMultiplicationSpec extends FlatSpec with Matchers {

  "multiply" should "multiply 2 matrices of dimensions 1x1 correctly" in {
    MatrixMultiplication.multiply(Array(Array(2)), Array(Array(3))) shouldEqual Array(Array(6))
  }

  it should "multiply 2 matrices of dimensions 2x2 return a matrix of 2x2" in {
    val a = Array(Array(1, 2), Array(3, 4))
    val b = Array(Array(5, 6), Array(7, 8))
    MatrixMultiplication.multiply(a, b) shouldEqual Array(Array(19, 22), Array(43, 50))
  }

  it should "multiply 2 matrices of dimensions 2x3 and 3x2 and return a matrix of 2x2" in {
    val a = Array(Array(1, 2, 3), Array(4, 5, 6))
    val b = Array(Array(7, 8), Array(9, 10), Array(11, 12))
    MatrixMultiplication.multiply(a, b) shouldEqual Array(Array(58, 64), Array(139, 154))
  }

  it should "return the same matrix when multiplied by the identity matrix" in {
    val a = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
    val i = Array(Array(1, 0, 0), Array(0, 1, 0), Array(0, 0, 1))
    MatrixMultiplication.multiply(a, i) shouldEqual a
  }

  "strassen" should "yield the same results using Strassen algorithm that you get with the iterative method" in {
    val a = Array(Array(1, 2), Array(3, 4))
    val b = Array(Array(5, 6), Array(7, 8))
    MatrixMultiplication.strassen(a, b) shouldEqual MatrixMultiplication.multiply(a, b)
  }

  it should "multiply 2 matrices of dimensions 4x4 return a matrix of 4x4" in {
    val a = Array(Array(5, 2, 6, 1), Array(0, 6, 2, 0), Array(3, 8, 1, 4), Array(1, 8, 5, 6))
    val b = Array(Array(7, 5, 8, 0), Array(1, 8, 2, 6), Array(9, 4, 3, 8), Array(5, 3, 7, 9))
    MatrixMultiplication.strassen(a, b) shouldEqual Array(Array(96, 68, 69, 69), Array(24, 56, 18, 52), Array(58, 95, 71, 92), Array(90, 107, 81, 142))
  }

  it should "multiply 2 matrices of dimensions 8x8 return a matrix of 8x8" in {
    val a = Array.fill[Array[Int]](8)(Array.fill[Int](8)(scala.util.Random.nextInt(10)))
    val b = Array.fill[Array[Int]](8)(Array.fill[Int](8)(scala.util.Random.nextInt(10)))
    MatrixMultiplication.strassen(a, b) shouldEqual MatrixMultiplication.multiply(a, b)
  }

  it should "confirm that the Strassen algorithm is faster" in {
    def time[R](block: => R): R = {
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) + "ns")
      result
    }

    val n = 8
    val a = Array.fill[Array[Int]](n)(Array.fill[Int](n)(scala.util.Random.nextInt(10)))
    val b = Array.fill[Array[Int]](n)(Array.fill[Int](n)(scala.util.Random.nextInt(10)))

    val r1 = time { MatrixMultiplication.multiply(a, b) }
    val r2 = time { MatrixMultiplication.strassen(a, b) }

    r1 shouldEqual r2
  }

  "sum" should "compute the sum of 2 arrays" in {
    val a = Array(Array(1, 2), Array(3, 4))
    val b = Array(Array(5, 6), Array(7, 8))
    MatrixMultiplication.sum(a, b) shouldEqual Array(Array(6, 8), Array(10, 12))
  }

  "subtract" should "compute the subtraction of 2 arrays" in {
    val a = Array(Array(1, 2), Array(3, 4))
    val b = Array(Array(5, 6), Array(7, 8))
    MatrixMultiplication.subtract(a, b) shouldEqual Array(Array(-4, -4), Array(-4, -4))
  }
}
