import scala.collection.mutable._

class Graph {

  var g = new Redis()

  def addNode(v: String): Int = {
    g.addEmptyList(v)
  }

  def addEdge(u: String, v: String): Int = {
    if (g.getListKeys().contains(u) && g.getListKeys().contains(u)) {
      g.lpush(u, v)
      g.lpush(v, u)
    }
    return 0
  }

  def adjacent(v: String): List[String] = {
    g.listGet(v)
  }

  def shortestPath(u: String, v: String): List[String] = {
    // predecessor[i] array stores predecessor of
    // i and distance array stores distance of i
    // from s
    var pred = HashMap[String, String]()
    var dist = HashMap[String, Int]()

    if (bfsSearch(u, v, g.getLengthOfLists(), pred, dist) == false) {
      throw new Exception("no possible path")
    }

    // vector path stores the shortest path
    var path = ListBuffer[String]()
    var crawl = v
    path += crawl
    while (pred(crawl) != null) {
      path += pred(crawl);
      crawl = pred(crawl);
    }

    return path.toList.reverse
  }

  def bfsSearch( start: String, end: String, numNodes: Int, pred: HashMap[String, String], dist: HashMap[String, Int]) {

    // a queue to maintain queue of vertices whose
    // adjacency list is to be scanned as per normal
    // DFS algorithm
    var queue = Queue[String]()

    // boolean array visited[] which stores the
    // information whether ith vertex is reached
    // at least once in the Breadth first search
    var visited = HashMap[String, Boolean]()

    val keySet = g.getListKeys()

    // initially all vertices are unvisited
    // so v[i] for all i is false
    // and as no path is yet constructed
    // dist[i] for all i set to infinity
    keySet.foreach((s) => {
      visited += (s -> false)
      dist += (s -> 2147483647) //the maximum in in scala
      pred += (s -> null)
    })

    // now source is first to be visited and
    // distance from source to itself should be 0
    visited(start) = true
    dist(start) = 0
    queue.enqueue(start)

    // standard BFS algorithm
    while (!queue.isEmpty) {
      val u = queue.front
      queue.dequeue()
      adjacent(u).foreach((s) => {
        if (visited(s) == false) {
          visited(s) = true
          dist(s) = dist(s) + 1
          pred(s) = u
          queue.enqueue(s)

          if (s == end) {
            return true
          }
        }
      })
    }
      return false
  }

  def main() = {
    val g = new Graph()

    g.addNode("x")
    g.addNode("j")
    g.addNode("b")
    g.addNode("f")
    g.addNode("r")
    g.addNode("c")
    g.addNode("e")
    g.addNode("y")

    g.addEdge("x", "j")
    g.addEdge("j", "x")
    g.addEdge("j", "f")
    g.addEdge("j", "r")
    g.addEdge("j", "b")
    g.addEdge("b", "j")
    g.addEdge("b", "f")
    g.addEdge("b", "r")
    g.addEdge("b", "c")
    g.addEdge("f", "j")
    g.addEdge("f", "b")
    g.addEdge("f", "e")
    g.addEdge("r", "j")
    g.addEdge("r", "b")
    g.addEdge("r", "c")
    g.addEdge("r", "y")
    g.addEdge("r", "e")
    g.addEdge("c", "b")
    g.addEdge("c", "r")
    g.addEdge("e", "r")
    g.addEdge("e", "f")
    g.addEdge("e", "y")
    g.addEdge("y", "r")
    g.addEdge("y", "e")

    g.shortestPath("x", "y")
  }

}
