package week1

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by mario on 10-10-16.
 */
class IntegerMultiplicationSpec  extends FlatSpec with Matchers  {

  "multiply" should "multiply two numbers using the iterative method" in {
    IntegerMultiplication.multiply("1", "1") shouldEqual 1
    IntegerMultiplication.multiply("7", "8") shouldEqual 56
    IntegerMultiplication.multiply("12", "10") shouldEqual 120
    IntegerMultiplication.multiply("1234", "5678") shouldEqual 7006652
    IntegerMultiplication.multiply("999", "99") shouldEqual 98901
    IntegerMultiplication.multiply("99", "999") shouldEqual 98901
  }

}
