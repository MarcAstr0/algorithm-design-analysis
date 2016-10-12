package assignments

/**
 * Created by mcastro on 10/12/2016.
 */
object AssignmentTwo {

  def quickSort1(arr: Array[Int]): Int = {
    var comparisons = 0
    def swap(i: Int, j: Int) = {
      val t = arr(i); arr(i) = arr(j); arr(j) = t
    }
    def partition(l: Int, r: Int): Unit = {
      if (r-l > 1) {
        comparisons += r-l
        val p = l
        swap(l, p)
        val pivot = arr(l)
        var i = l + 1
        for {j <- l + 1 to r} {
          if (arr(j) < pivot) {
            swap(j, i)
            i += 1
          }
        }
        swap(l, i-1)
        if (l < i) partition(l, i)
        if (i < r) partition(i, r)
      }
    }
    partition(0, arr.length-1)
    comparisons
  }

  def main(args: Array[String]): Unit = {
    val stream = getClass.getResourceAsStream("/assignments/QuickSort.txt")
    val lines = scala.io.Source.fromInputStream(stream).getLines
    val inputArray = (for { line <- lines } yield line.toInt).toArray

    println(quickSort1(inputArray))
  }
}
