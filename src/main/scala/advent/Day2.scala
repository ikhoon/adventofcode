package advent

import scala.io.Source
import scala.math.min

/**
  * Created by ikhoon on 2015. 12. 27..
  */



object Day2 extends App {

  case class Box(l: Int, w: Int, h: Int)

  val pattern = """(\d+)x(\d+)x(\d+)""".r
  val sizes = getFile("/day2/input.txt").getLines()
  val boxes = sizes.flatMap {
    pattern.findAllIn(_).matchData.map { m => Box(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt) }
  }



  val totalWrappingPaper = boxes.foldLeft(0) { case (acc , Box(l, w, h)) => {
    val (lw, wh, hl) = (l*w, w*h, h*l)
    acc + (lw + wh + hl) * 2 + min(min(lw, wh), hl)
  }}
  println(s"total : $totalWrappingPaper")

}
