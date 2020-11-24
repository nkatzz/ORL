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

package trail.learning.utils

import com.typesafe.scalalogging.Logger
import trail.app.runutils.{Example, InputDataParser, RunningOptions}
import trail.inference.ASPSolver
import trail.logic.{Clause, Constant, Literal}

import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by nkatz at 17/4/20
  */

object LearnUtils {

  /*def main(args: Array[String]) = {
    val resultsPath = "/home/nkatz/dev/vagmcs/BK/move/crossval-results"
    //val resultsPath = "/home/nkatz/dev/vagmcs/BK/rendezVous/crossval-results"
    getFinalF1Score(resultsPath)
  }*/

  def parseTheoryFromFile(inps: RunningOptions, filePath: String) = {
      def matches(p: Regex, str: String) = p.pattern.matcher(str).matches

    if (filePath == "") Nil
    else {
      val source = Source.fromFile(filePath)
      val list = source.getLines.filter(line => !matches("""""".r, line) && !line.startsWith("%"))
      val modes = inps.globals.modeHs ++ inps.globals.modeBs
      val rulesList = list.map(x => Clause.parse(x, modes)).toList
      source.close
      rulesList
    }
  }

  def readDataToExmpl(dataPath: String, inps: RunningOptions, logger: Logger) = {
    def matches(p: Regex, str: String) = p.pattern.matcher(str).matches
    val source = Source.fromFile(dataPath)
    val list = source.getLines.filter(line => !matches("""""".r, line) && !line.startsWith("%")).toList
    val tostr = list.mkString(" ")

    val (queryAtoms, observationAtoms) = InputDataParser.parseData(tostr, inps)
    source.close
    if (queryAtoms.isEmpty) logger.warn("No query atoms found!")
    Example(queryAtoms.map(_.tostring), observationAtoms.map(_.tostring), "0")

    /*val (annotation, narrative) = InputHandling.splitData(list, inps.targetConcepts)
    source.close
    Example(annotation.toList, narrative.toList, "0")*/
  }

  def setTypePredicates(newRules: List[Clause], inps: RunningOptions) = {
    val mh = inps.globals.modeHs
    val mb = inps.globals.modeBs
    newRules.map(_.setTypeAtoms(mh ++ mb))
  }

  def setTypePredicates(rule: Clause, inps: RunningOptions) = {
    val mh = inps.globals.modeHs
    val mb = inps.globals.modeBs
    rule.setTypeAtoms(mh ++ mb)
  }

  def getFinalF1Score(resultsFile: String) = {
    val source = Source.fromFile(resultsFile)
    val list = source.getLines.filter(x => x.startsWith("TPs")).toList
    val f = (x: String) => x.split(": ")(1).toInt
    val split = list.map(x => x.split(",")).map(x => (f(x(0)), f(x(1)), f(x(2))))
    val (tps, fps, fns) = split.foldLeft(0, 0, 0) { (x, y) => (x._1 + y._1, x._2 + y._2, x._3 + y._3) }
    val precision = tps.toDouble / (tps.toDouble + fps.toDouble)
    val recall = tps.toDouble / (tps.toDouble + fns.toDouble)
    val f1score = (2 * precision * recall) / (precision + recall)
    println(precision, recall, f1score)
  }
}
