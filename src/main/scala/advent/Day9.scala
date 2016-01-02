package advent

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

/**
  * Created by ikhoon on 2016. 1. 1..
  */
object Day9 extends App {

  case class Edge(name: String)

  case class Route(edges: (Edge, Edge), distance: Int)

  type Distance = (Node, Int)

  class Node(val edge: Edge, var adjacency: List[Distance])

  object Node { def apply(edge: Edge) = new Node(edge, Nil) }

  object DistanceParser extends RegexParsers {

    private def location: Parser[String] = """[a-zA-Z]+""".r

    private def distance : Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }

    private def route : Parser[Route] = location ~ "to" ~ location ~ "=" ~ distance ^^ { case l1 ~ _ ~ l2 ~ _ ~ d => Route((Edge(l1), Edge(l2)), d) }

    def get(str: String): Route = parse(route, str).get

  }

  val paths = mutable.Set[List[(Edge, Int)]]()

  def dfs(node: Node, path: List[(Edge, Int)]): Unit = node.adjacency.foreach {
      case (next, dst) => {
        if(path.forall{ case (e, d) => next.edge != e} && path.length < edges.length) dfs(next, (next.edge, dst) :: path )
        else if(path.length == edges.length) paths += path
        else paths
      }
  }

  def printPath(path : (List[String], Int)) = println(s"${path._1.reverse.mkString(" -> ")} = ${path._2}")

  val distances = getFile("/day9/input.txt").getLines().toList
  val routes = distances.map(DistanceParser.get)
  val edges = routes.map(route => route.edges).flatMap(edges => List(edges._1, edges._2)).distinct
  val nodes = (Map[Edge, Node]() /: routes) {
    case (node , route) => {
      val (edge1, edge2)= (route.edges._1, route.edges._2)
      val (node1, node2) = (node.getOrElse(edge1, Node(edge1)), node.getOrElse(edge2, Node(edge2)))
      val distance = route.distance
      node1.adjacency = (node2, distance) :: node1.adjacency
      node2.adjacency = (node1, distance) :: node2.adjacency
      node + (edge1 -> node1, edge2 -> node2)
    }
  }

  nodes.values.foreach { node => dfs(node, List((node.edge, 0))) }

  val shortest = paths.map(_.foldLeft((List[String](), 0)) { case ((cities, acc), (edge, dist)) => (edge.name :: cities, acc + dist)})

  shortest.foreach(printPath)
  println()
  printPath(shortest.minBy {case (_, dist) => dist})

}
