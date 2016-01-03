package advent

import scala.annotation.tailrec

/**
  * Created by ikhoon on 2016. 1. 3..
  */
object Day11 extends App {

  @tailrec
  def increment(pw: String, carry: Int = 0): String = {
    val pos = pw.length - carry - 1
    if(pw(pos) != 'z') pw.take(pos) + (pw(pos) + 1).toChar + pw.drop(pos + 1)
    else increment(pw.take(pos) + 'a' + pw.drop(pos + 1), carry + 1)
  }

  def rule1(pw: String) : Boolean = ((List[Int](), ' ') /: pw) {
    case ((Nil, _), char) => (List(1), char)
    case ((counts, prev), char) if char - prev == 1 => ((counts.head + 1) :: counts.tail, char)
    case ((counts, _), char) => (1 :: counts, char)
  }._1.max >= 3

  def rule2(pw: String) : Boolean = pw.count("iol".contains(_)) == 0

  def rule3(pw: String) : Boolean = ((List[Int](), ' ') /: pw) {
    case ((Nil, _), char) => (List(1), char)
    case ((counts, prev), char) if char == prev => ((counts.head + 1) :: counts.tail, char)
    case ((counts, _), char) => (1 :: counts, char)
  }._1.count(_ >= 2) >= 2

  def predicate(pw: String): Boolean = rule1(pw) && rule2(pw) && rule3(pw)

  @tailrec
  def next(pw: String) : String = {
    val candidate = increment(pw)
    if(predicate(candidate)) candidate else next(candidate)
  }

  assert(next("abcdefgh") == "abcdffaa")
  assert(next("ghijklmn") == "ghjaabcc")
  println(next("cqjxjnds"))
}
