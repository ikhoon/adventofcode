package advent

import scala.annotation.tailrec

/**
  * Created by ikhoon on 2016. 1. 3..
  */
object Day10 extends App {

  def look(str: String) = (List[(Char, Int)]() /: str) {
    case (Nil, c) => List((c, 1))
    case (acc, c) => if(acc.head._1 == c) (c, acc.head._2 + 1) :: acc.tail else (c, 1) :: acc
  }.reverse

  def say(looked : List[(Char, Int)]) = ("" /: looked) { case (acc, (char, num)) => acc + num.toString + char }

  val lookAndSay = say _ compose look

  @tailrec
  def repeat(count: Int, str: String): String = if(count == 0) str else repeat(count - 1, lookAndSay(str))

  assert(lookAndSay("1") == "11")
  assert(lookAndSay("11") == "21")
  assert(lookAndSay("21") == "1211")
  assert(lookAndSay("1211") == "111221")
  assert(lookAndSay("111221") == "312211")
  assert(repeat(5, "1") == "312211")

  println(repeat(40, "3113322113").length)

}
