package week1

import scala.annotation.tailrec

/**
 * Created by mario on 03-10-16.
 */
object Sorting {

  def mergeSort(arr: Array[Int]): Array[Int] = {
    def merge(a: Array[Int], b: Array[Int]): Array[Int] = {
      val n = a.length + b.length
      var c = Array.fill[Int](n)(0)
      var i, j = 0
      for (k <- 0 to n-1) {
        if (i > a.length-1) {
          c(k) = b(j)
          j += 1
        } else if (j > b.length-1) {
          c(k) = a(i)
          i += 1
        } else {
          if (a(i) <= b(j)) {
            c(k) = a(i)
            i += 1
          } else if (a(i) > b(j)) {
            c(k) = b(j)
            j += 1
          }
        }
      }
      c
    }

    if (arr.length == 1) arr
    else {
      val a = arr.take((arr.length/2.0).floor.toInt)
      val b = arr.takeRight((arr.length/2.0).ceil.toInt)
      merge(mergeSort(a), mergeSort(b))
    }
  }
}
