package selection

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by mcastro on 10/17/2016.
 */
class SelectionSpec extends FlatSpec with Matchers {

  "randomizedSelect()" should "return the i-th smallest element of the input array" in {
    Selection.randomizedSelect(Array(1), 1) shouldEqual 1
    Selection.randomizedSelect(Array(10, 8, 2, 4), 1) shouldEqual 2
    Selection.randomizedSelect(Array(10, 8, 2, 4), 2) shouldEqual 4
    Selection.randomizedSelect(Array(10, 8, 2, 4), 3) shouldEqual 8
    Selection.randomizedSelect(Array(10, 8, 2, 4), 4) shouldEqual 10
  }

}
