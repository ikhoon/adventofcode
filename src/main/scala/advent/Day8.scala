package advent

/**
  * Created by ikhoon on 2016. 1. 1..
  */
object Day8 extends App {
  def memoryValueSize(str: String): Int= str.foldLeft((0, "")) {
    case ((acc, ""), '\"')  => (acc, "")
    case ((acc, ""), '\\')  => (acc, "\\")
    case ((acc, bslash@"\\"), '\\') => (acc + 1, "")
    case ((acc, quote@"\\" ), '\"') => (acc + 1, "")
    case ((acc, bslash@"\\"), x@'x') => (acc, bslash + x)
    case ((acc, ascii@"\\x"), hex) => (acc, ascii + hex)
    case ((acc, ascii), hex) if ascii.startsWith("\\x") && ascii.length == 3 => (acc + 1, "")
    case ((acc, _) ,_) => (acc + 1, "")
  }._1
  
  assert(memoryValueSize(""""byc\x9dyxuafof\\\xa6uf\\axfozomj\\olh\x6a"""") == 29)
  assert(memoryValueSize("\"\"") == 0)
  assert(memoryValueSize("\"abc\"") == 3)
  assert(memoryValueSize("\"aaa\\\"aaa\"") == 7)
  assert(memoryValueSize("\"\\x27\"") == 1)

  val diff = getFile("/day8/input.txt").getLines().foldLeft(0) { case (acc, line) => acc + line.length - memoryValueSize(line) }

  println(diff)
}
