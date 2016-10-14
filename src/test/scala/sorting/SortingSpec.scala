package sorting

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by mario on 03-10-16.
 */
class SortingSpec extends FlatSpec with Matchers {
  var stream = getClass.getResourceAsStream("/assignments/10.txt")
  var lines = scala.io.Source.fromInputStream(stream).getLines
  val input10 = (for { line <- lines } yield line.toInt).toArray

  stream = getClass.getResourceAsStream("/assignments/100.txt")
  lines = scala.io.Source.fromInputStream(stream).getLines
  val input100 = (for { line <- lines } yield line.toInt).toArray

  stream = getClass.getResourceAsStream("/assignments/1000.txt")
  lines = scala.io.Source.fromInputStream(stream).getLines
  val input1000 = (for { line <- lines } yield line.toInt).toArray

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

  it should "correctly sort the test inputs" in {
    Sorting.mergeSort(input10) shouldEqual input10.sortWith(_ < _)
    Sorting.mergeSort(input100) shouldEqual input100.sortWith(_ < _)
    Sorting.mergeSort(input1000) shouldEqual input1000.sortWith(_ < _)
  }

  "quickSort()" should "return the same array for an array with only one element" in {
    Sorting.quickSort(Array(1)) shouldEqual Array(1)
  }

  it should "return the same array if the array is already sorted" in {
    Sorting.quickSort(Array(1, 2, 3)) shouldEqual Array(1, 2, 3)
  }

  it should "return the sorted array" in {
    Sorting.quickSort(Array(3, 5, 1, 4, 2)) shouldEqual Array(1, 2, 3, 4, 5)
  }

  it should "return the sorted array when an array has repeated elements" in {
    Sorting.quickSort(Array(3, 5, 2, 4, 2)) shouldEqual Array(2, 2, 3, 4, 5)
  }

  it should "correctly sort the test inputs" in {
    Sorting.quickSort(input10) shouldEqual input10.sortWith(_ < _)
    Sorting.quickSort(input100) shouldEqual input100.sortWith(_ < _)
    Sorting.quickSort(input1000) shouldEqual input1000.sortWith(_ < _)
  }

  "mergeSort() and quickSort()" should "return the same sorted array" in {
    Sorting.quickSort(input10) shouldEqual Sorting.mergeSort(input10)
    Sorting.quickSort(input100) shouldEqual Sorting.mergeSort(input100)
    Sorting.quickSort(input1000) shouldEqual Sorting.mergeSort(input1000)
  }
}
