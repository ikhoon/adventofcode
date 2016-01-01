package advent


import advent.Day7.CircuitParser.circuit
import advent.Day7.Emulator.{connect, probe}

import scala.collection.mutable

/**
  * Created by ikhoon on 2016. 1. 1..
  */
object Day7 extends App {

  object Emulator {

    trait Expr { def eval: Int }

    case class Val(v: Int) extends Expr { override lazy val eval: Int = v }

    case class Var(name : String) extends Expr { override lazy val eval: Int = register.getOrElse(this, Val(0)).eval }

    case class And(l: Expr, r: Expr) extends Expr { override lazy val eval: Int = l.eval & r.eval }

    case class Or(l: Expr, r: Expr) extends Expr { override lazy val eval: Int = l.eval | r.eval }

    case class LShift(l: Expr, r: Expr) extends Expr { override lazy val eval: Int = l.eval << r.eval }

    case class RShift(l: Expr, r: Expr) extends Expr { override lazy val eval: Int = l.eval >> r.eval }

    case class Not(v: Expr) extends Expr { override lazy val eval: Int = 65535 - v.eval }

    case class Circuit(expr: Expr, variable: Var) extends Expr {
      override lazy val eval: Int = { expr.eval }
    }

    val register = mutable.Map[Var, Expr]()

    def probe(wire: String): Int = register.getOrElse(Var(wire), Val(0)).eval

    def connect(circuits: List[Circuit]): Unit = {
      register.clear()
      circuits.foreach(circuit => register += circuit.variable -> circuit.expr)
    }
  }


  import scala.util.parsing.combinator._
  object CircuitParser extends RegexParsers {
    import advent.Day7.Emulator._

    private def -> : Parser[String] = "->"

    private def variable : Parser[Var] = """[a-zA-Z]+""".r ^^ { v => Var(v)}

    private def binary : Parser[String] = "AND" | "OR" | "LSHIFT" | "RSHIFT"

    private def num: Parser[Expr] = """(0|[1-9]\d*)""".r ^^ { i => Val(i.toInt) }

    private def value : Parser[Expr] = variable | num

    private def not : Parser[Not] = "NOT" ~ value ^^ { case _ ~ v => Not(v) }

    private def operate : Parser[Expr] = value ~ binary ~ value ^^ {
      case l ~ "AND" ~ r => And(l, r)
      case l ~ "OR" ~ r => Or(l, r)
      case l ~ "LSHIFT" ~ r => LShift(l, r)
      case l ~ "RSHIFT" ~ r => RShift(l, r)
    } | not


    private def left : Parser[Expr] = operate | value

    private def right: Parser[Var] = variable

    private def builder: Parser[Circuit] = left ~ -> ~ right ^^ { case l ~ _ ~ r => Circuit(l, r)}

    def circuit(str: String): Circuit = parse(builder, str).get

  }

  val circuits1 = List(circuit("123 -> x"), circuit("123 -> x"), circuit("456 -> y"), circuit("x AND y -> d"),
    circuit("x OR y -> e"), circuit("x LSHIFT 2 -> f"), circuit("y RSHIFT 2 -> g"), circuit("NOT x -> h"), circuit("NOT y -> i"))

  connect(circuits1)
  assert(probe("d") == 72)
  assert(probe("e") == 507)
  assert(probe("f") == 492)
  assert(probe("g") == 114)
  assert(probe("x") == 123)
  assert(probe("y") == 456)
  assert(probe("h") == 65412)
  assert(probe("i") == 65079)

  val circuits2 = getFile("/day7/input.txt").getLines().map(circuit).toList
  connect(circuits2)
  print(probe("a"))
}

