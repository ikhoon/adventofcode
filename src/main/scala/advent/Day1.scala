package advent

import scala.io.Source

/**
  * Created by ikhoon on 2015. 12. 27..
  */
object Day1 extends App {

  val floor = getFile("/day1/input.txt").iter.foldLeft(0) { (acc, c) => c match {
      case '(' => acc + 1
      case ')' => acc - 1
    }
  }
  println(s"floor: $floor")

}
