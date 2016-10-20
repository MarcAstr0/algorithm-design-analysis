package graphs

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by mcastro on 10/19/2016.
 */
class GraphsSpec extends FlatSpec with Matchers {

  "countVertices" should "return the number of vertices in a graph represented by and adjacency list" in {
    Graph.countVertices(Map(1 -> List())) shouldEqual 1
    Graph.countVertices(Map(1 -> List(2, 3), 2 -> List(1, 3), 3 -> List(1, 2))) shouldEqual 3
  }

  "countEdges" should "return the number of edges in a graph represented by and adjacency list" in {
    Graph.createEdgeList(Map(1 -> List())).size shouldEqual 0
    Graph.createEdgeList(Map(1 -> List(1))).size shouldEqual 0
    Graph.createEdgeList(Map(1 -> List(2, 3), 2 -> List(1, 3), 3 -> List(1, 2))).size shouldEqual 3
    Graph.createEdgeList(Map(1 -> List(2, 3), 2 -> List(1, 2, 3), 3 -> List(1, 2))).size shouldEqual 3
  }

  "createAdjacencyList" should "create an adjacency list from an input text file" in {
    val al1 = getClass.getResourceAsStream("/graphs/adjacencyList1.txt")
    Graph.createAdjacencyList(al1, " ") shouldEqual Map(
      1 -> List(2, 3, 4, 7),
      2 -> List(1, 3, 4),
      3 -> List(1, 2, 4),
      4 -> List(1, 2, 3, 5),
      5 -> List(4, 6, 7, 8),
      6 -> List(5, 7, 8),
      7 -> List(1, 5, 6, 8),
      8 -> List(5, 6, 7)
    )
  }

  "minCut" should "compute the min cut of the graph represented by the adjacency list" in {
    val al1 = getClass.getResourceAsStream("/graphs/adjacencyList1.txt")
    val al2 = getClass.getResourceAsStream("/graphs/adjacencyList2.txt")
    val al3 = getClass.getResourceAsStream("/graphs/adjacencyList3.txt")
    val al4 = getClass.getResourceAsStream("/graphs/adjacencyList4.txt")
    val al5 = getClass.getResourceAsStream("/graphs/adjacencyList5.txt")
    val al6 = getClass.getResourceAsStream("/graphs/adjacencyList6.txt")
    Graph.minCut(Graph.createAdjacencyList(al1, " ")) shouldEqual 2
    Graph.minCut(Graph.createAdjacencyList(al2, " ")) shouldEqual 2
    Graph.minCut(Graph.createAdjacencyList(al3, " ")) shouldEqual 1
    Graph.minCut(Graph.createAdjacencyList(al4, " ")) shouldEqual 1
    Graph.minCut(Graph.createAdjacencyList(al5, " ")) shouldEqual 3
    Graph.minCut(Graph.createAdjacencyList(al6, " ")) shouldEqual 2
  }
}
