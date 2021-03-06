package graphs

import java.io.InputStream

import scala.collection.immutable.Queue

/**
 * Created by mcastro on 10/19/2016.
 */
object Graph {

  def countVertices(adjList: Map[Int, List[Int]]): Int = adjList.size

  def createEdgeList(adjList: Map[Int, List[Int]]): List[(Int, Int)] = {
    {
      for {
        x <- adjList.transform((a, b) => b.map(c => if (a < c) (a, c) else (c, a) )).values.toList
        y <- x} yield y
    }.distinct.filter(a => a._1 != a._2)
  }

  def createAdjacencyList(input: InputStream, separator: String): Map[Int, List[Int]] = {
    val al = for {
      line <- scala.io.Source.fromInputStream(input).getLines
    } yield line.split(separator).head.toInt -> line.split(separator).map(x => x.toInt).tail.toList
    al.toMap
  }

  def createAdjacencyListWithWeights(input: InputStream, separator: String): Map[Int, List[(Int, Int)]] = {
    val al = for {
      line <- scala.io.Source.fromInputStream(input).getLines
    } yield line.split(separator).head.toInt -> line.split(separator).tail
        .map(x => (x.split(",").head.toInt, x.split(",").tail.head.toInt))
        .toList
    al.toMap
  }

  def minCutOnce(adjList: Map[Int, List[Int]]): Int = {
    var el = createEdgeList(adjList)
    var minCut = el.length
    def contract(edgeList: List[(Int, Int)], u: Int, v: Int): List[(Int, Int)] = {
      edgeList.map(x =>
        if (x._1 == u) (v, x._2)
        else if (x._2 == u) (x._1, v)
        else x
      ).filter(x => x._1 != x._2)
    }
    while (el.length > 2) {
      val seed = scala.util.Random.nextInt(el.length)
      el = contract(el, el(seed)._1, el(seed)._2)
      if (el.length > 0) minCut = el.length
    }
    minCut
  }

  def minCut(adjList: Map[Int, List[Int]]): Int = {
    var minCut = minCutOnce(adjList)
    for {i <- 0 to 1000} {
      val m = minCutOnce(adjList)
      if (minCutOnce(adjList) < minCut) minCut = m
    }
    minCut
  }

  def bfs(adjList: Map[Int, List[Int]], s: Int): List[Int] = {
    val explored = collection.mutable.Map(adjList.map{case (k, v) => k -> false}.toSeq: _*)
    explored(s) = true
    val q = new scala.collection.mutable.Queue[Int]()
    q.enqueue(s)
    while (!q.isEmpty) {
      val v = q.dequeue
      for {w <- adjList(v)} {
        if (!explored(w)) {explored(w) = true; q.enqueue(w)}
      }
    }
    explored.map{case (x, y) => x}.toList
  }

  def connectedComponents(adjList: Map[Int, List[Int]]): List[List[Int]] = {
    val explored = collection.mutable.Map(adjList.map{case (k, v) => k -> false}.toSeq: _*)
    def bfs(adjList: Map[Int, List[Int]], s: Int): List[Int] = {
      explored(s) = true
      val result = scala.collection.mutable.ArrayBuffer[Int]()
      result += s
      val q = new scala.collection.mutable.Queue[Int]()
      q.enqueue(s)
      while (!q.isEmpty) {
        val v = q.dequeue
        for {w <- adjList(v)} {
          if (!explored(w)) {explored(w) = true; result += w; q.enqueue(w)}
        }
      }
      result.toList
    }

    for {
      i <- adjList.map{case (k, v) => k}.toList
      if (!explored(i))
    } yield bfs(adjList, i).sorted
  }

  def getWeightOfEdge(adjList: Map[Int, List[(Int, Int)]], u: Int, v: Int): Int = adjList(u).filter(x => x._1 == v).head._2

  def getNeighbors(adjList: Map[Int, List[(Int, Int)]], u: Int): List[Int] = adjList(u).map(x => x._1)

  def dijkstra(adjList: Map[Int, List[(Int, Int)]]): Map[Int, Int] = {
    val infinity = 1000000
    val source = 1
    val v = scala.collection.mutable.ListBuffer[Int]() // visited vertices
    val vertices = for {(vertex, edges)  <- adjList} yield if (vertex == source) (vertex -> 0) else (vertex -> infinity)
    val q = collection.mutable.Map() ++ vertices // our vertex set
    v += source
    var current = source
    while (q.size != v.length) {
      val unvisitedNeighbors = getNeighbors(adjList, current).filter(!v.contains(_))
      for {uv <- unvisitedNeighbors} {
        q(uv) = Math.min(q(uv), q(current) + getWeightOfEdge(adjList, current, uv))
      }
      current = q.filterKeys(!v.contains(_)).toSeq.sortBy(_._2).toList.head._1
      v += current
    }
    collection.immutable.Map() ++ q
  }

}
