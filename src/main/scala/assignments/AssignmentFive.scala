package assignments

import graphs.Graph

/**
 * Created by mcastro on 11/10/2016.
 */
object AssignmentFive {

  def main(args: Array[String]): Unit = {
    val input = getClass.getResourceAsStream("/assignments/dijkstraData.txt")
    val g = Graph.createAdjacencyListWithWeights(input, "\t")
    val paths = Graph.dijkstra(g)
    val vertices = List(7,37,59,82,99,115,133,165,188,197)
    val answer = for {v <- vertices} yield paths(v)
    println(answer.mkString(","))
  }

}
