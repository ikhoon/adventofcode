package advent

/**
  * Created by ikhoon on 2015. 12. 30..
  */
object Day5 extends App {
  lazy val restrictedChars = List("ab", "cd", "pq", "xy")

  def rule1(str: String): Boolean = str.count("aeiou" contains _) >= 3

  def rule2(str: String): Boolean = str.zip(str.substring(1)).count(c => c._1 == c._2) > 0

  def rule3(str: String): Boolean = restrictedChars.count(str contains _) == 0

  def predicate(str: String): Boolean = rule1(str) && rule2(str) && rule3(str)

  assert(predicate("ugknbfddgicrmopn"))
  assert(predicate("aaa"))
  assert(!predicate("jchzalrnumimnmhp"))
  assert(!predicate("haegwjzuvuyypxyu"))
  assert(!predicate("dvszwmarrgswjxmb"))

  println(getFileInputStream("/day5/input.txt").getLines().count(predicate))
}
