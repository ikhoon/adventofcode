package advent

import org.apache.commons.codec.binary.Hex
import org.apache.commons.codec.digest.DigestUtils

/**
  * Created by ikhoon on 2015. 12. 28..
  */
object Day4 extends App {

  def md5(string: String): Array[Byte] = DigestUtils.md5(string)
  def toHex(bytes: Array[Byte]): String = Hex.encodeHexString(bytes)
  def matched(hex: String): Boolean = hex.startsWith("00000")

  val predicate = matched _ compose toHex compose md5
  def mining(prefix: String): Int= Stream.from(0).dropWhile(i => !predicate(s"$prefix$i")).head

  assert(mining("abcdef") == 609043)
  assert(mining("pqrstuv") == 1048970)
  println(mining("ckczppom"))
}


