package selection

/**
 * Created by mcastro on 10/17/2016.
 */
object Selection {

  def randomizedSelect(arr: Array[Int], o: Int): Int = {
    def swap(i: Int, j: Int) = {
      val t = arr(i); arr(i) = arr(j); arr(j) = t
    }
    def partition(l: Int, r: Int, o: Int): Int = {
      if (r - l == 0) {
        arr(l)
      } else {
      val p = scala.util.Random.nextInt(r - l) + l
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
      val j = i-1
      if (j == o) arr(j)
      else if (j > o) partition(l, j-1, o)
      else partition(j+1, r, o-j)
      }
    }
    partition(0, arr.length-1, o-1)
  }

}
