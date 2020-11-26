package cli

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.funsuite.AnyFunSuite
import trail.app.LearnRevise

class LearnRevSpecs extends AnyFunSuite with LazyLogging {

  val exmplsDir = s"${System.getProperty("user.dir")}/examples"

  /*test("learnrev - theory induction with yale shooting") {
    val yale = s"$exmplsDir/yale"
    val args = Array(s"--inpath=$yale", s"--train=$yale/data")
    LearnRevise.main(args)
  }

  test("learnrev - theory induction for yale shooting from command line") {
    import scala.sys.process._
    val yale = s"$exmplsDir/yale"
    val args = Array(s"--inpath=$yale", s"--train=$yale/data")
    val options = args.mkString(" ")
    s"trail learnrev $options".lineStream_!
  }*/

}
