/*
 * Copyright (C) 2016  Nikos Katzouris
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package trail.inference

import com.typesafe.scalalogging.LazyLogging
import trail.app.runutils.{Example, Globals}
import trail.logic.Clause
import trail.logic.parsers.ClausalLogicParser
import trail.app.utils.Utils.dumpToFile

import scala.sys.process._

/**
  * Created by nkatz at 6/12/19
  */

object ASPSolver extends ClausalLogicParser with LazyLogging {

  /**
    * Calls Clingo and returns the results.
    */
  def solve(program: String, options: String = ""): Vector[String] = {

      def aspResult: Parser[Vector[String]] = repsep(literal, "") ^^ { x => x.map(_.tostring).toVector }

      def processLine(x: String) = {
        val stripped = x.replaceAll("\\s", "")
        parseAll(aspResult, stripped) match {
          case Success(result, _) => result
          case _ => Vector.empty[String]
        }
      }

    val file = dumpToFile(program)
    val filePath = file.getCanonicalPath
    val aspCores = s"-t${Runtime.getRuntime.availableProcessors}"

    val clingo = Globals.clingo
    val command = {
      if (options == "") Seq(clingo, filePath, "0", "-Wno-atom-undefined", aspCores) //, "--time-limit=20"
      else Seq(clingo, filePath, options, "-Wno-atom-undefined", aspCores) //, "--time-limit=10"
    }

    val res = command.mkString(" ").lineStream_!
    val results = res.toVector

    val stop = "stop"

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
      if (options.contains("--opt-mode=enum,")) {
        return Vector("UNSATISFIABLE")
      } else {
        logger.error(s"\n\nUNSATISFIABLE PROGRAM:\n\n$program")
        System.exit(-1)
      }
    }

    val answerSet = results.map(x => processLine(x)).filter(_.nonEmpty) //.reverse

    if (answerSet.isEmpty) {
      Vector.empty[String]
    } else if (answerSet.size > 1) {
      // This happens when the --opt-mode=optN option is used, in which case all optimal
      // solutions are returned. We then transform the separate solutions into one string
      // to be returned and "decoded" to the solutions by the TheoryRevision.revise method,
      // which is the only point in the code where --opt-mode=optN may be used.
      if (!options.contains("--opt-mode=optN")) {
        answerSet.reverse.head
      } else {
        answerSet.map(solution => solution.mkString("<@>"))
      }
    } else {
      answerSet.head
    }
  }

  //return all optimal and pick one downstream.

  def crispLogicInference(theory: List[Clause], e: Example, globals: Globals) = {
    val modes = globals.modeHs ++ globals.modeBs
    val t = theory.map(x => x.withTypePreds(modes).tostring).mkString("\n")
    //val program = e.toASP().mkString("\n") + t + "\n" + s"""#include "${globals.BK_WHOLE_EC}".""" + "\n" + "\n#show.\n#show holdsAt/2.\n" + "#show initiatedAt/2.\n" ++ "#show terminatedAt/2.\n"
    val program = e.toASP().mkString("\n") + t + "\n" + globals.BK + "\n" + "\n#show.\n#show holdsAt/2.\n" + "#show initiatedAt/2.\n" ++ "#show terminatedAt/2.\n"

    val results = solve(program)
    results.map(x => x -> true).toMap
  }

}
