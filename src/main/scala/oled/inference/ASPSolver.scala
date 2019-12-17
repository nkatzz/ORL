package oled.inference

import com.typesafe.scalalogging.LazyLogging
import oled.logic.parsers.ClausalLogicParser
import oled.utils.Utils.dumpToFile
import scala.sys.process._

/**
 * Created by nkatz at 6/12/19
 */

object ASPSolver extends ClausalLogicParser with LazyLogging {

  /**
   * Calls Clingo and returns the results.
   * */
  def solve(program: String) = {

    def aspResult: Parser[List[String]] = repsep(literal, "") ^^ { x => x.map(_.tostring).toList}//.toVector }

    def processLine(x: String) = {
      val stripped = x.replaceAll("\\s", "")
      parseAll(aspResult, stripped) match {
        case Success(result, _) => result
        case f => List.empty[String]
      }
    }

    val file = dumpToFile(program)
    val filePath = file.getCanonicalPath
    val aspCores = s"-t${Runtime.getRuntime.availableProcessors}"
    val command = Seq("clingo", filePath, "0", "-Wno-atom-undefined", aspCores)

    val res = command.mkString(" ").lineStream_!
    val results = res.toVector

    val status = {
      val statusLine = results.filter(x =>
        x.contains("SATISFIABLE") || x.contains("UNSATISFIABLE") || x.contains("OPTIMUM FOUND"))
      if (statusLine.isEmpty) {
        logger.error(s"\nNo STATUS returned from Clingo for program:\n\n$program")
        System.exit(-1)
      }
      // extract the actual string literal (SATISFIABLE, UNSATISFIABLE or OPTIMUM FOUND)
      statusLine.head.replaceAll("\\s", "")
    }

    if (status == "UNSATISFIABLE") {
      logger.error(s"\n\nUNSATISFIABLE PROGRAM:\n\n$program")
      System.exit(-1)
    }

    val answerSet = results.map(x => processLine(x)).filter(_.nonEmpty)//.reverse

    if (answerSet.isEmpty) {
      List.empty[String]
    } else if (answerSet.size > 1) {
      // If this happens for some reason, it should be checked-out.
      throw new RuntimeException(s"More than one answer sets returned from Clingo. The answer sets are: ${answerSet.foreach(println)}")
    } else {
      answerSet.head
    }
  }

}
