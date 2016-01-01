package advent

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable

/**
  * Created by ikhoon on 2015. 12. 31..
  */
object Day6 extends App{

  import scala.util.parsing.combinator._

  case class Pos(x: Int, y: Int)
  case class Range(from: Pos, to: Pos)
  case class Command(action: String, range: Range)

  val (on, off, toggle) = ("turn on", "turn off", "toggle")

  object CommandParser extends RegexParsers {
    def apply(str: String) = parse(command, str)
    private def action : Parser[String] = "turn on" | "turn off" | "toggle"

    private def num : Parser[Int] = """(0|[1-9]\d*)""".r ^^ { _.toInt }

    private def pos : Parser[Pos] = num ~ "," ~ num ^^ { case x ~ _ ~ y => Pos(x,y)}

    private def range : Parser[Range] = pos ~ "through" ~ pos ^^ { case f ~ _ ~ t => Range(f,t)}

    private def command : Parser[Command] = action ~ range ^^ { case a ~ r => Command(a, r)}

  }

  class Decoration {
    private val lights = mutable.Map[Pos, Boolean]()
    def apply(command: Command): Decoration = {
      val positions = getPositions(command.range)
      command.action match {
        case `on` => positions.foreach(lights.update(_, true))
        case `off` => positions.foreach(lights.update(_, false))
        case `toggle` => positions.foreach(pos => lights.update(pos, !lights.getOrElse(pos, false)))
      }
      this
    }
    def count(isOn: Boolean) = lights.count(_._2 == isOn)

    private def getPositions(range: Range) = for {
      x <- range.from.x to range.to.x
      y <- range.from.y to range.to.y
    } yield Pos(x, y)

  }
  object Decoration {
    def apply(): Decoration = new Decoration()(Command("turn off", Range(Pos(0,0),Pos(999,999))))
  }

  val decoration = Decoration()
  val total = decoration.count(false)
  val command1 = CommandParser("turn on 0,0 through 999,999").get
  assert(decoration(command1).count(true) == total)
  val command2 = CommandParser("toggle 0,0 through 999,0").get
  assert(decoration(command2).count(true) == total - 1000)
  val command3 = CommandParser("turn off 499,499 through 500,500").get
  assert(decoration(command3).count(true) == total - 1000 - 4)


  val result = getFile("/day6/input.txt").getLines().foldLeft(Decoration()) {
    case (deco, line) => deco(CommandParser(line) get)
  }.count(true)

  println(result)


}
