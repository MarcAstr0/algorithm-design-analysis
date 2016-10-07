package week1

import org.scalatest.{Matchers, FlatSpec}

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
}
