package assignments

import assignments.AssignmentTwo._
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by mcastro on 10/12/2016.
 */
class AssignmentTwoSpecs extends FlatSpec with Matchers {
  var stream = getClass.getResourceAsStream("/assignments/10.txt")
  var lines = scala.io.Source.fromInputStream(stream).getLines
  val input10 = (for { line <- lines } yield line.toInt).toArray

  stream = getClass.getResourceAsStream("/assignments/100.txt")
  lines = scala.io.Source.fromInputStream(stream).getLines
  val input100 = (for { line <- lines } yield line.toInt).toArray

  stream = getClass.getResourceAsStream("/assignments/1000.txt")
  lines = scala.io.Source.fromInputStream(stream).getLines
  val input1000 = (for { line <- lines } yield line.toInt).toArray

  "test files" should "have the right size" in {
    input10.length shouldEqual 10
    input100.length shouldEqual 100
    input1000.length shouldEqual 1000
  }

  "quickSort1" should "return the correct number of comparisons when the pivot is the first element of the array" ignore {
    AssignmentTwo.quickSort1(input10) shouldEqual 25
    AssignmentTwo.quickSort1(input100) shouldEqual 615
    AssignmentTwo.quickSort1(input1000) shouldEqual 10297
  }

  "quickSort2" should "return the correct number of comparisons when the pivot is the last element of the array" ignore {
    AssignmentTwo.quickSort2(input10) shouldEqual 29
    AssignmentTwo.quickSort2(input100) shouldEqual 587
    AssignmentTwo.quickSort2(input1000) shouldEqual 10184
  }

  "quickSort3" should "return the correct number of comparisons when the pivot is the median of three" in {
    AssignmentTwo.quickSort3(input10) shouldEqual 21
    AssignmentTwo.quickSort3(input100) shouldEqual 518
    AssignmentTwo.quickSort3(input1000) shouldEqual 8921
  }
}
