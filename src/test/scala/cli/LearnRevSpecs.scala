package cli

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.funsuite.AnyFunSuite

class LearnRevSpecs extends AnyFunSuite with LazyLogging {

  test("learnrev - theory induction") {

    println(System.getProperty("user.dir"))
    val currentDirectory = new java.io.File(".").getCanonicalPath
    println(currentDirectory)
  }

}
