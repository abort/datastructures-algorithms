import scala.io.StdIn

object Evacuation {
  // n = cities
  // m = roads
  // u = start of road
  // v = end of road
  // c = number of people that can be transported through this road in 1h
  final def main(args: Array[String]): Unit = {
    var line = StdIn.readLine.split(" ").map(_.toInt) // n m
    val n = line(0)
    val m = line(1)
    val graph = new FlowGraph(n)

    for (_ <- 1 until m) {
      line = StdIn.readLine.split(" ").map(_.toInt) // u v c
      val u = line(0)
      val v = line(1)
      val c = line(2)
      graph.addEdge(u, v, c)
    }

    println("Computing max flow")
    println(computeMaxFlow(1, n, graph))
  }


  private def computeMaxFlow(source : Integer, sink : Integer, graph : FlowGraph): Integer = {
    var flow = 0
    var running = true
    while (running) {
      println("compute path")
      val path = graph.findShortestPath(source, sink)

      println(s"path: ${path.mkString(" ")}")
      running = path.nonEmpty
      if (running) {
        var distance = Integer.MAX_VALUE
        println("Compute min distance")
        path.foreach { e =>
          distance = Math.min(distance, e.capacity - e.flow)
        }

        println("Update flow")
        path.foreach { e =>
          graph.addFlow(e.id, distance)
        }

        flow = flow + distance
      }
    }
    flow
  }

  type Vertex = Int
  case class Edge(id : Int, source : Vertex, sink : Vertex, capacity : Int, var flow : Int = 0)

  class FlowGraph(val size : Int) {
    private var id = 0
    private var visited = Set.empty[Vertex]
    var edges: IndexedSeq[Edge] = IndexedSeq.empty
    var graph: Map[Vertex, Seq[Edge]] = (0 to size).indices.map(_ -> Seq.empty[Edge]).toMap

    def addEdge(v : Vertex, w : Vertex, capacity : Int): Unit = {
      val forwardEdge = Edge(id, v, w, capacity)
      val backwardEdge = Edge(id + 1, w, v, 0)
      edges = edges :+ forwardEdge :+ backwardEdge
      graph = graph.updated(v, graph(v) :+ forwardEdge)
      graph = graph.updated(w, graph(w) :+ backwardEdge)
      id = id + 2
    }

    def out(v : Vertex) : Seq[Vertex] = graph(v).map(_.sink)

    def addFlow(edgeId : Int, flow : Int) : Unit = {
      edges(edgeId).flow += flow
      edges(edgeId + 1).flow -= flow
    }

    def findShortestPath(source : Vertex, sink : Vertex) : Seq[Edge] = {
      var toVisit = Seq(source)
      var path = Seq.empty[Edge]
      while (toVisit.nonEmpty) {
        val v = toVisit.head
        toVisit = toVisit.tail
        for (e <- graph(v)) {
          if (!visited(e.sink) && e.sink != e.source && e.capacity > e.flow) {
            path = path :+ e
            toVisit = toVisit :+ e.sink
            visited = visited + e.sink
          }
        }
      }
      path
    }
  }
}
