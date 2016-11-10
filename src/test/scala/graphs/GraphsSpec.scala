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

  "bfs" should "return the vertices reachable by starting vertex s using breadth-first search" in {
    val bfs1 = getClass.getResourceAsStream("/graphs/bfs1.txt")
    Graph.bfs(Graph.createAdjacencyList(bfs1, " "), 1).sorted shouldEqual List(1, 2, 3, 4, 5, 6).sorted
//    Graph.bfs(Graph.createAdjacencyList(bfs1, " "), 6).sorted shouldEqual List(1, 2, 3, 4, 5, 6).sorted
  }

  "connectedComponents" should "return the connected components of a graph using breadth-frist search" in {
    val bfs2 = getClass.getResourceAsStream("/graphs/bfs2.txt")
    Graph.connectedComponents(Graph.createAdjacencyList(bfs2, " ")) should contain theSameElementsAs List(List(1, 3, 5, 7, 9), List(2, 4), List(6, 8, 10))
  }

  "getWeightOfEdge" should "return the weight of the edge in a weighted graph" in {
    val dijkstra = getClass.getResourceAsStream("/graphs/dijkstra1.txt")
    val g = Graph.createAdjacencyListWithWeights(dijkstra, " ")
    Graph.getWeightOfEdge(g, 1, 2) shouldEqual 1
    Graph.getWeightOfEdge(g, 1, 8) shouldEqual 2
    Graph.getWeightOfEdge(g, 2, 1) shouldEqual 1
    Graph.getWeightOfEdge(g, 2, 3) shouldEqual 1
    Graph.getWeightOfEdge(g, 3, 2) shouldEqual 1
    Graph.getWeightOfEdge(g, 3, 4) shouldEqual 1
    Graph.getWeightOfEdge(g, 4, 3) shouldEqual 1
    Graph.getWeightOfEdge(g, 4, 5) shouldEqual 1
    Graph.getWeightOfEdge(g, 5, 4) shouldEqual 1
    Graph.getWeightOfEdge(g, 5, 6) shouldEqual 1
    Graph.getWeightOfEdge(g, 6, 5) shouldEqual 1
    Graph.getWeightOfEdge(g, 6, 7) shouldEqual 1
    Graph.getWeightOfEdge(g, 7, 6) shouldEqual 1
    Graph.getWeightOfEdge(g, 7, 8) shouldEqual 1
    Graph.getWeightOfEdge(g, 8, 7) shouldEqual 1
    Graph.getWeightOfEdge(g, 8, 1) shouldEqual 2
  }

  "getNeighbors" should "return the neighboring vertices for a given vertex" in {
    val dijkstra = getClass.getResourceAsStream("/graphs/dijkstra1.txt")
    val g = Graph.createAdjacencyListWithWeights(dijkstra, " ")
    Graph.getNeighbors(g, 1) shouldEqual List(2, 8)
    Graph.getNeighbors(g, 2) shouldEqual List(1, 3)
    Graph.getNeighbors(g, 3) shouldEqual List(2, 4)
    Graph.getNeighbors(g, 4) shouldEqual List(3, 5)
    Graph.getNeighbors(g, 5) shouldEqual List(4, 6)
    Graph.getNeighbors(g, 6) shouldEqual List(5, 7)
    Graph.getNeighbors(g, 7) shouldEqual List(6, 8)
    Graph.getNeighbors(g, 8) shouldEqual List(7, 1)
  }

  "dijkstra" should "return the shortest path for a given graph and destination" in {
    val dijkstra = getClass.getResourceAsStream("/graphs/dijkstra1.txt")
    val g = Graph.createAdjacencyListWithWeights(dijkstra, " ")
    val paths = Graph.dijkstra(g)
    paths(1) shouldEqual 0
    paths(2) shouldEqual 1
    paths(3) shouldEqual 2
    paths(4) shouldEqual 3
    paths(5) shouldEqual 4
    paths(6) shouldEqual 4
    paths(7) shouldEqual 3
    paths(8) shouldEqual 2
  }
}
