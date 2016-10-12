package assignments

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by mcastro on 10/6/2016.
 */
class AssignmentOneSpec extends FlatSpec with Matchers {

  "countInversions()" should "return 0 for a sorted array" in {
    AssignmentOne.countInversions(Array(1, 2, 3, 4), 0)._2 shouldEqual 0
  }

  it should "count the correct number of inversions" in {
    AssignmentOne.countInversions(Array(1, 3, 5, 2, 4, 6), 0)._2 shouldEqual 3
    AssignmentOne.countInversions(Array(2, 4, 1, 3, 5), 0)._2 shouldEqual 3
    AssignmentOne.countInversions(Array(6, 5, 4, 3, 2, 1), 0)._2 shouldEqual 15
  }
}
