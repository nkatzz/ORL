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

package asp

import com.typesafe.scalalogging.LazyLogging
import org.scalatest.funsuite.AnyFunSuite
import trail.app.runutils.{ClingoDirectives, ModesInfoParser}
import utils.ModesHandler.getModes

class ClingoCoverageDirectivesSpecs extends AnyFunSuite with LazyLogging {

  val modes =
    """
      |examplePattern(holdsAt(move(+person,+person),+time))
      |examplePattern(holdsAt(meet(+person,+person),+time))
      |
      |modeh(initiatedAt(move(+person,+person),+time))
      |modeh(terminatedAt(move(+person,+person),+time))
      |modeh(initiatedAt(meet(+person,+person),+time))
      |modeh(terminatedAt(meet(+person,+person),+time))
      |
      |%modeb(holdsAt(move(+person,+person),+time))
      |%modeb(not holdsAt(move(+person,+person),+time))
      |
      |modeb(happensAt(enter(+person),+time))
      |modeb(happensAt(exit(+person),+time))
      |modeb(happensAt(walking(+person),+time))
      |modeb(happensAt(active(+person),+time))
      |modeb(happensAt(inactive(+person),+time))
      |modeb(happensAt(running(+person),+time))
      |%modeb(happensAt(close(+person,+person,#threshold_value),+time))
      |modeb(close(+person,+person,#threshold_value,+time))
      |modeb(far(+person,+person,#threshold_value,+time))
      |modeb(orientationMove(+person,+person,+time))
      |modeb(orientationFar(+person,+person,+time))
      |
      |inputPredicate(happensAt(walking(+person),+time))
      |inputPredicate(happensAt(active(+person),+time))
      |inputPredicate(happensAt(inactive(+person),+time))
      |inputPredicate(happensAt(running(+person),+time))
      |inputPredicate(happensAt(enter(+person),+time))
      |inputPredicate(happensAt(exit(+person),+time))
      |inputPredicate(close(+person,+person,#threshold_value,+time))
      |inputPredicate(orientationMove(+person,+person,+time))
      |
      |comparisonPredicate(close(+person,+person,#threshold_value,+time), lessThan, comparison_term_position(3))
      |comparisonPredicate(far(+person,+person,#threshold_value,+time), greaterThan, comparison_term_position(3))
      |
      |""".stripMargin

  test("Generating coverage directives for Clingo from the mode declarations") {
    val f = trail.app.utils.Utils.dumpToFile(modes)
    val mip = new ModesInfoParser(f.getCanonicalPath)
    val cd = new ClingoDirectives(mip)
    logger.info("\n" + cd.tps_fps_fns_tns_defs.mkString("\n"))
    logger.info("\n" + cd.hardCoverageConstrs.mkString("\n"))
    logger.info("\n" + cd.minimizeStatements.mkString("\n"))
  }

}
