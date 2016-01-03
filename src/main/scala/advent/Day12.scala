package advent

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.fasterxml.jackson.module.scala.experimental.ScalaObjectMapper

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


/**
  * Created by ikhoon on 2016. 1. 3..
  */
object Day12 extends App {

  def findAllNumbers(jsonNode: JsonNode): List[Int]= jsonNode match {
    case json if json.isArray || json.isObject => {
      val buf = ListBuffer[Int]()
      val elements = json.elements()
      while (elements.hasNext) buf ++= findAllNumbers(elements.next)
      buf.toList
    }
    case json if json.isNumber => List(json.asInt())
    case _ => Nil
  }

  val mapper = new ObjectMapper with ScalaObjectMapper

  val json1 = mapper.readValue("[1,2,3]", classOf[JsonNode])
  val json2 = mapper.readValue("{\"a\":2,\"b\":4}", classOf[JsonNode])
  val json3 = mapper.readValue("[[[3]]]", classOf[JsonNode])
  val json4 = mapper.readValue("{\"a\":{\"b\":4},\"c\":-1}", classOf[JsonNode])
  val json5 = mapper.readValue("{\"a\":[-1,1]}", classOf[JsonNode])
  val json6 = mapper.readValue("[-1,{\"a\":1}]", classOf[JsonNode])

  assert(findAllNumbers(json1).sum == 6)
  assert(findAllNumbers(json2).sum == 6)
  assert(findAllNumbers(json3).sum == 3)
  assert(findAllNumbers(json4).sum == 3)
  assert(findAllNumbers(json5).sum == 0)
  assert(findAllNumbers(json6).sum == 0)

  val jsonNode = mapper.readValue(getFile("/day12/input.txt").getLines().mkString(""), classOf[JsonNode])
  println(findAllNumbers(jsonNode).sum)
}
