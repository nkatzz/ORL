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

package orl.learning.woledasp

import com.typesafe.scalalogging.LazyLogging
import orl.app.runutils.CMDArgs
import orl.datahandling.InputHandling.{FileDataOptions, MongoDataOptions, getMongoData}
import orl.datahandling.{Example, InputHandling}
import orl.learning.TheoryTester
import orl.logic.Clause

import scala.io.Source

/**
  * Created by nkatz at 17/2/20
  */

object CrossValTest extends LazyLogging {

  /**
    * To use this its necessary to manually set the type predicates
    * in the getTypePredicates method of ASPWeightedInference.
    */

  def main(args: Array[String]) = {
    val argsok = CMDArgs.argsOk(args)
    if (argsok._1) {

      val runningOptions = CMDArgs.getOLEDInputArgs(args)

      val source = Source.fromFile("/home/nkatz/tmp/test.lp")
      val rules = source.getLines.
        filter(x => (x.replaceAll("\\s", "") != "") && !x.startsWith("%")).toList.map { line =>

          println(line)

          val split = line.split(" ")
          val weight = split(0)
          val rule = line.split(weight + " ")(1)
          val clause = Clause.parse(rule)
          clause.weight = weight.toDouble
          clause
        }

      source.close

      val testingDataOptions1 = new MongoDataOptions(
        dbNames       = MeetingTrainTestSets.meeting4._2,
        chunkSize     = runningOptions.chunkSize,
        targetConcept = runningOptions.targetHLE,
        sortDbByField = "time", what = "testing"
      )
      val testingDataFunction1: MongoDataOptions => Iterator[Example] = getMongoData

      val testingDataOptions = FileDataOptions(
        filepath       = runningOptions.test,
        chunkSize      = runningOptions.chunkSize,
        targetConcept  = runningOptions.targetHLE,
        setting        = "testing",
        sortByFunction = (x: String) => x.split(',').last.replaceAll("\\)", "").trim.toInt
      )
      val testingDataFunction: FileDataOptions => Iterator[Example] = InputHandling.getFileData

      val data = testingDataFunction(testingDataOptions)

      // ASP
      //ASPWeightedInference.evalOnTestSet(data, rules, runningOptions)

      val tester = new TheoryTester(data, rules, runningOptions, true)
      tester.testTheory

      // MLN
      //WoledMLNLearnerUtils.evalOnTestSet(data, rules, runningOptions)
    } else {
      logger.error(argsok._2)
      System.exit(-1)
    }
  }

}
