package sorting

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by mario on 03-10-16.
 */
class SortingSpec extends FlatSpec with Matchers {

  "mergeSort()" should "return the same array for an array with only one element" in {
      Sorting.mergeSort(Array(1)) shouldEqual Array(1)
    }

  it should "return the same array if the array is already sorted" in {
    Sorting.mergeSort(Array(1, 2, 3)) shouldEqual Array(1, 2, 3)
  }

  it should "return the sorted array" in {
    Sorting.mergeSort(Array(3, 5, 1, 4, 2)) shouldEqual Array(1, 2, 3, 4, 5)
  }

  it should "return the sorted array when an array has repeated elements" in {
    Sorting.mergeSort(Array(3, 5, 2, 4, 2)) shouldEqual Array(2, 2, 3, 4, 5)
  }
}
