import scala.io.Source.fromInputStream
import scala.io.{BufferedSource, Source}

/**
  * Created by ikhoon on 2015. 12. 28..
  */
package object advent {

  def getFile(file: String): BufferedSource = fromInputStream(getClass.getResourceAsStream(file))
}
