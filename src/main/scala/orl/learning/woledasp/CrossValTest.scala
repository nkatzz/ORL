package orl.learning.woledasp

import com.typesafe.scalalogging.LazyLogging
import orl.app.runutils.CMDArgs
import orl.app.runutils.InputHandling.MongoDataOptions
import orl.datahandling.Example
import orl.learning.woledmln.WoledMLNLearnerUtils
import orl.logic.{Clause, Literal}
import orl.app.runutils.InputHandling.getMongoData

import scala.io.Source

/**
  * Created by nkatz at 17/2/20
  */

object Drafts extends App {

  val x = Literal.parse("p(X)").negated

  println(x.tostring)

  println(x.negated.tostring)

}

object CrossValTest extends LazyLogging{

  def main(args: Array[String]) = {
    val argsok = CMDArgs.argsOk(args)
    if (argsok._1) {

      val runningOptions = CMDArgs.getOLEDInputArgs(args)

      val source = Source.fromFile("/home/nkatz/tmp/test.lp")
      val rules = source.getLines.filter(x => (x.replaceAll("\\s", "") != "" ) && !x.startsWith("%")).toList.map { line =>
        val split = line.split(" ")
        val weight = split(0)
        val rule = line.split(weight+" ")(1)
        val clause = Clause.parse(rule)
        clause.weight = weight.toDouble
        clause
      }

      source.close

      val testingDataOptions =
        new MongoDataOptions(dbNames = MeetingTrainTestSets.meeting1._2,
          chunkSize = runningOptions.chunkSize, targetConcept = runningOptions.targetHLE, sortDbByField = "time", what = "testing")

      val testingDataFunction: MongoDataOptions => Iterator[Example] = getMongoData

      val data = testingDataFunction(testingDataOptions)

      //val m = new ASPWeightedInference(rules, data.next(), runningOptions)
      //m.performInference()
      //println("ok")

      WoledMLNLearnerUtils.evalOnTestSet(data, rules, runningOptions)

      val test = "stop"

    } else {
      logger.error(argsok._2)
      System.exit(-1)
    }
  }




}
