package week1

import scala.collection.mutable.ArrayBuffer

/**
 * Created by mcastro on 10/7/2016.
 */
object MatrixMultiplication {

  def multiply(a: Array[Array[Int]], b: Array[Array[Int]]): Array[Array[Int]] = {
    val c = Array.fill[Array[Int]](a.length)(Array.fill[Int](b(0).length)(0))
    for {k <- 0 to a.length-1} {
      for {i <- 0 to a.length-1} {
        for {j <- 0 to b.length-1} {
          c(k)(i) += a(k)(j)*b(j)(i)
        }
      }
    }
    c
  }

  def strassen(x: Array[Array[Int]], y: Array[Array[Int]]): Array[Array[Int]] = {
    if (x.length == 2 && y.length == 2) {
      val p1 = x(0)(0) * (y(0)(1) - y(1)(1))
      val p2 = (x(0)(0) + x(0)(1)) * y(1)(1)
      val p3 = (x(1)(0) + x(1)(1)) * y(0)(0)
      val p4 = x(1)(1) * (y(1)(0) - y(0)(0))
      val p5 = (x(0)(0) + x(1)(1)) * (y(0)(0) + y(1)(1))
      val p6 = (x(0)(1) - x(1)(1)) * (y(1)(0) + y(1)(1))
      val p7 = (x(0)(0) - x(1)(0)) * (y(0)(0) + y(0)(1))
      Array(Array(p5 + p4 - p2 + p6, p1 + p2), Array(p3 + p4, p1 + p5 - p3 - p7))
    } else {
      val topX = x.slice(0, x.length/2)
      val bottomX = x.slice(x.length/2, x.length)
      val topY = y.slice(0, y.length/2)
      val bottomY = y.slice(y.length/2, y.length)
      var a, b, c, d, e, f, g, h, r = ArrayBuffer[Array[Int]]()

      // Sub-matrices for X
      for {i <- 0 until topX.length} { a += topX(i).slice(0, topX(i).length/2) } // A
      for {i <- 0 until topX.length} { b += topX(i).slice(topX(i).length/2, topX(i).length) } // B
      for {i <- 0 until bottomX.length} { c += bottomX(i).slice(0, bottomX(i).length/2) } // C
      for {i <- 0 until bottomX.length} { d += bottomX(i).slice(bottomX(i).length/2, bottomX(i).length) } // D
      // Sub-matrices for Y
      for {i <- 0 until topY.length} { e += topY(i).slice(0, topY(i).length/2) } // E
      for {i <- 0 until topY.length} { f += topY(i).slice(topX(i).length/2, topY(i).length) } // F
      for {i <- 0 until bottomY.length} { g += bottomY(i).slice(0, bottomY(i).length/2) } // G
      for {i <- 0 until bottomY.length} { h += bottomY(i).slice(bottomY(i).length/2, bottomY(i).length) } // H

      // The 7 products
      val p1 = strassen(a.toArray, subtract(f.toArray, h.toArray))
      val p2 = strassen(sum(a.toArray, b.toArray), h.toArray)
      val p3 = strassen(sum(c.toArray, d.toArray), e.toArray)
      val p4 = strassen(d.toArray, subtract(g.toArray, e.toArray))
      val p5 = strassen(sum(a.toArray, d.toArray), sum(e.toArray, h.toArray))
      val p6 = strassen(subtract(b.toArray, d.toArray), sum(g.toArray, h.toArray))
      val p7 = strassen(subtract(a.toArray, c.toArray), sum(e.toArray, f.toArray))

      val r1 = sum(subtract(sum(p5, p4), p2), p6)
      val r2 = sum(p1, p2)
      val r3 = sum(p3, p4)
      val r4 = subtract(subtract(sum(p1, p5), p3), p7)

      // we join in a single matrix
      for {i <- 0 to r1.length-1} {r += r1(i) ++ r2(i) }
      for {i <- 0 to r3.length-1} {r += r3(i) ++ r4(i) }
      r.toArray
    }
  }

  def sum(x: Array[Array[Int]], y: Array[Array[Int]]): Array[Array[Int]] = {
    val result = Array.fill[Array[Int]](x.length)(Array.fill[Int](x(0).length)(0))
    for {i <- 0 to x.length-1} {
      for {j <- 0 to x.length-1} {
        result(i)(j) = x(i)(j) + y(i)(j)
      }
    }
    result
  }

  def subtract(x: Array[Array[Int]], y: Array[Array[Int]]): Array[Array[Int]] = {
    val result = Array.fill[Array[Int]](x.length)(Array.fill[Int](x(0).length)(0))
    for {i <- 0 to x.length-1} {
      for {j <- 0 to x.length-1} {
        result(i)(j) = x(i)(j) - y(i)(j)
      }
    }
    result
  }

}
