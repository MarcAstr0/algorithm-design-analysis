package assignments

import graphs.Graph

/**
 * Created by mcastro on 10/20/2016.
 */
object AssignmentThree {

  def main(args: Array[String]): Unit = {
    val input = getClass.getResourceAsStream("/assignments/kargerMinCut.txt")
    println(Graph.minCut(Graph.createAdjacencyList(input, "\\t")))
  }

}
