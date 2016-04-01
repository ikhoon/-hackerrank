package algorithm

import scala.collection.mutable

/**
  * Created by ikhoon on 2016. 3. 24..
  */
object EvenTree extends App {
  case class Node(value: Int, edges: List[Node])

  val graph = Map[Int, Node]()

  def connect(nodes: Map[Int, Node], a: Int, b: Int): Map[Int, Node] = {
    val nodeA = nodes.getOrElse(a, Node(a, Nil))
    val nodeB = nodes.getOrElse(b, Node(b, Nil))
    nodes
      .updated(a,nodeA.copy(edges = nodeB :: nodeA.edges))
      .updated(b,nodeB.copy(edges = nodeA :: nodeB.edges))
  }

//  def evenize(node: Node, visited: Set[Node]): Node = {
//  }

  def getChildSize(node: Node, visited: mutable.Set[Node]): Int = {
    if(visited.contains(node)) 0
    else {
      val a: List[Int] = node.edges.map(getChildSize(_, visited += node))

      0

    }
  }



}
