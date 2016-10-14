package sorting

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Created by mario on 03-10-16.
 */
object Sorting {

  /** Function that sorts an array using the MergeSort algorithm
   *
   * @param arr the array we want to sort
   * @return the sorted array
   */
  def mergeSort(arr: Array[Int]): Array[Int] = {
    def merge(a: Array[Int], b: Array[Int]): Array[Int] = {
      val n = a.length + b.length
      val c = Array.fill[Int](n)(0)
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

  /** Function that sorts an array using the QuickSort algorithm
   *
   * This implementation uses the median-of-three method to select the pivot
   *
   * @param arr the array we want to sort
   * @return the sorted array
   */
  def quickSort(arr: Array[Int]): Array[Int] = {
    def swap(i: Int, j: Int) = {
      val t = arr(i); arr(i) = arr(j); arr(j) = t
    }
    def partition(l: Int, r: Int): Unit = {
      if (r == l || r < l) {
        // do nothing
      } else {
        val first = arr(l)
        val last = arr(r)
        val middle = arr((r+l)/2)
        val median = Array(first, middle, last).sortWith(_ < _).drop(1).head
        if (median == middle) { val p = (r+l)/2; swap(l, p) }
        else if (median == first) { val p = l; swap(l, p) }
        else { val p = r; swap(l, p) }
        val pivot = arr(l)
        var i = l + 1
        for {j <- l + 1 to r} {
          if (arr(j) < pivot) {
            swap(j, i)
            i += 1
          }
        }
        swap(l, i-1)
        if (l < i) partition(l, i-2)
        if (i < r) partition(i, r)
      }
    }
    partition(0, arr.length-1)
    arr
  }
}
