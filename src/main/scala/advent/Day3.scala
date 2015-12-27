package advent

import scala.collection.mutable

/**
  * Created by ikhoon on 2015. 12. 28..
  */
object Day3 extends App {
  case class Pos(x: Int, y: Int)
  val moves = getFileInputStream("/day3/input.txt").iter
  val start = Pos(0, 0)
  var paths = mutable.Map[Pos, Int](start -> 1)
  val updatePath = (pos: Pos) => {
    paths += pos -> (paths.getOrElse(pos, 0) + 1)
    pos
  }
  moves.foldLeft(start) { (prev, move) => move match {
      case '^' => updatePath(Pos(prev.x, prev.y + 1))
      case '>' => updatePath(Pos(prev.x + 1, prev.y))
      case 'v' => updatePath(Pos(prev.x, prev.y - 1))
      case '<' => updatePath(Pos(prev.x - 1, prev.y))
    }
  }
  println(paths.size)
}
