package assignments

/**
 * Created by mcastro on 10/6/2016.
 */
object AssignmentOne {

  def countInversions(arr: Array[Int], inversions: BigInt): (Array[Int], BigInt) = {
    def merge(a: (Array[Int], BigInt), b: (Array[Int], BigInt)): (Array[Int], BigInt) = {
      val n = a._1.length + b._1.length
      val c = Array.fill[Int](n)(0)
      var i, j = 0
      var inv = a._2 + b._2
      for (k <- 0 to n-1) {
        if (i > a._1.length-1) {
          c(k) = b._1(j)
          j += 1
        } else if (j > b._1.length-1) {
          c(k) = a._1(i)
          i += 1
        } else {
          if (a._1(i) <= b._1(j)) {
            c(k) = a._1(i)
            i += 1
          } else if (a._1(i) > b._1(j)) {
            c(k) = b._1(j)
            j += 1
            inv += (a._1.length - i)
          }
        }
      }
      (c, inv)
    }

    if (arr.length == 1) (arr, 0)
    else {
      val a = arr.take((arr.length/2.0).floor.toInt)
      val b = arr.takeRight((arr.length/2.0).ceil.toInt)
      merge(countInversions(a, inversions), countInversions(b, inversions))
    }
  }

  def main(args: Array[String]): Unit = {
    val stream = getClass.getResourceAsStream("/assignments/IntegerArray.txt")
    val lines = scala.io.Source.fromInputStream(stream).getLines
    val inputArray = (for { line <- lines } yield line.toInt).toArray

    println(countInversions(inputArray, 0)._2)
  }
}
