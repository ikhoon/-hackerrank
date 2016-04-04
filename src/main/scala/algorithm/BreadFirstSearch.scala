package algorithm

import scala.collection.mutable.ArrayBuffer


/**
  * Created by ikhoon on 2016. 4. 3..
  */

object BreadFirstSearch extends App {


  import scala.collection.mutable
  case class Vertex(n: Int, var distance: Int, adjacency: mutable.Buffer[Vertex])

  type Distance = Int
  type Graph = Map[Int, Vertex]

  val UNIT_DISTANCE = 6
  def toTwoInt(str: String) : (Int, Int) = {
    val ab = str.split(" ")
    (ab.head.toInt, ab.last.toInt)
  }

  def getMinVertex(adjacency: mutable.Buffer[Vertex], q: mutable.Buffer[Int]): Option[Vertex] = {
    val next = adjacency.filter(vertex => q.contains(vertex.n))
    if (next.isEmpty) None else Some(next.minBy(_.distance))
  }

  def shortest(graph: Graph, start: Vertex, q: mutable.Buffer[Int]) : Unit = {
    var adj = mutable.Buffer[Vertex](start)
    while (q.nonEmpty) {
      val option = getMinVertex(adj, q)
      option match {
        case Some(c) =>
          q.remove(q.indexOf(c.n))
          c.adjacency.filter(adj => q.contains(adj.n)).foreach(adj => {
            val alternative  = c.distance + UNIT_DISTANCE
            if(alternative < adj.distance)
              adj.distance = alternative
          })
          adj = c.adjacency
        case _ => q.clear()
      }
    }
  }


  val input = io.Source.stdin.getLines()

  val t =  input.next().trim.toInt
  (1 to t).foreach(i => {
    val (n, m) = toTwoInt(input.next().trim)

    val graph = (1 to n).map(n => n -> Vertex(n, Int.MaxValue, mutable.ArrayBuffer[Vertex]())).toMap

    (1 to m).foreach(j => {
      val (a, b) = toTwoInt(input.next().trim)
      val nodeA = graph.get(a).get
      val nodeB = graph.get(b).get
      nodeA.adjacency += nodeB
      nodeB.adjacency += nodeA
    })
    val s = input.next().trim.toInt
    val start = graph.get(s).get
    start.distance = 0
    val q = (1 to n).toBuffer

    shortest(graph, start, q)

    val routes = (1 to n).filterNot(_ == s).map(p => {
      val node = graph(p)
      if (node.distance == Int.MaxValue) -1 else node.distance
    }).mkString(" ")
    println(routes)
  })
}
