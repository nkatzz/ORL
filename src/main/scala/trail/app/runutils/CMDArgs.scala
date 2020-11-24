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

package trail.app.runutils

import com.mongodb.casbah.MongoClient
import com.typesafe.scalalogging.LazyLogging
import trail.app.cliapp.Trail

import scala.collection.mutable

/**
  * Created by nkatz at 13/12/19
  */

object CMDArgs extends LazyLogging {

  private val map = scala.collection.mutable.Map[String, String]()

  def parseInputOptions(_args: Array[String]) = {

    val (args, task_or_usage) = _args.partition(_.contains("="))
    val (usage, _task) = task_or_usage.partition(_.contains("--usage"))
    val task = _task.mkString("")

    val split = args map { x => val z = x.replaceAll("\\s", "").split("="); (z(0), z(1)) }

      def getMatchingArgumentValue(argname: String): Any = {
        try {
          val arg = arguments.find(x => x.name == argname).getOrElse(throw new RuntimeException("Argument not found."))
          val value = arg.valueType match {
            case "String" => split.find(x => x._1 == arg.name).getOrElse(("", arg.default))._2.toString
            case "Int" => split.find(x => x._1 == arg.name).getOrElse(("", arg.default))._2.toInt
            case "Double" => split.find(x => x._1 == arg.name).getOrElse(("", arg.default))._2.toDouble
            case "Boolean" => split.find(x => x._1 == arg.name).getOrElse(("", arg.default))._2.toBoolean
            case _ => throw new RuntimeException("Don't know what to do with these arguments...")
          }
          map += argname -> value.toString
          value
        } catch {
          case _: RuntimeException => {
            logger.error(s"Argument $argname not found.")
            System.exit(-1)
          }
        }

      }

    val entryPath = getMatchingArgumentValue("--inpath")
    val delta = getMatchingArgumentValue("--delta")
    val pruningThreshold = getMatchingArgumentValue("--prune")
    val minSeenExmpls = getMatchingArgumentValue("--minseen")
    val specializationDepth = getMatchingArgumentValue("--spdepth")
    val breakTiesThreshold = getMatchingArgumentValue("--ties")
    val repeatFor = getMatchingArgumentValue("--repfor")
    val chunkSize = getMatchingArgumentValue("--chunksize")
    val onlinePruning = getMatchingArgumentValue("--onlineprune")
    val withPostPruning = getMatchingArgumentValue("--postprune")
    val tryMoreRules = getMatchingArgumentValue("--try-more-rules")
    val targetConcept = getMatchingArgumentValue("--target")
    val scoringFun = getMatchingArgumentValue("--scorefun")
    val minEvaluatedOn = getMatchingArgumentValue("--eval-atleast-on")
    val mintps = getMatchingArgumentValue("--min-pos-covered")
    val shuffleData = getMatchingArgumentValue("--shuffle-data")
    val showRefs = getMatchingArgumentValue("--showrefs")
    val pruneAfter = getMatchingArgumentValue("--prune-after")
    val tpWeight = getMatchingArgumentValue("--tps-weight")
    val fpWeight = getMatchingArgumentValue("--fps-weight")
    val fnWeight = getMatchingArgumentValue("--fns-weight")
    val withInertia = getMatchingArgumentValue("--inertia")
    val weightLearn = getMatchingArgumentValue("--weight-learning")
    val adagradDelta = getMatchingArgumentValue("--ada-delta")
    val adaLearnRate = getMatchingArgumentValue("--ada-learn-rate")
    val adaRegularization = getMatchingArgumentValue("--ada-regularization")
    val adaLossFunction = getMatchingArgumentValue("--ada-loss-function")
    val withEventCalculus = getMatchingArgumentValue("--with-ec")
    val saveTheoryTo = getMatchingArgumentValue("--saveto")
    val train = getMatchingArgumentValue("--train")
    val test = getMatchingArgumentValue("--test")
    val ruleLearningStrategy = getMatchingArgumentValue("--rule-learning-strategy")
    val infoGainAtLeast = getMatchingArgumentValue("--infogain")
    val inputTheory = getMatchingArgumentValue("--input-theory")
    val removeRules = getMatchingArgumentValue("--remove-rules")
    val debug = getMatchingArgumentValue("--debug")
    val findAllOpt = getMatchingArgumentValue("--optall")
    var clingo = getMatchingArgumentValue("--clingo").toString
    val perfectFit = getMatchingArgumentValue("--perfect-fit") //.toString
    val data = getMatchingArgumentValue("--data")
    val inferenceMode = getMatchingArgumentValue("--mode")

    // Check if Clingo is OK
    import scala.sys.process._
    try {
      // Default path for clingo is $TRAIL_HOME/dependencies/clingo/build/bin/clingo
      s"${clingo.toString} --version".lineStream_!
      Globals.clingo = clingo.toString
    } catch {
      case _: java.io.IOException =>
        try {
          // If the above doesn't work try to see if clingo is in the path
          val t = s"clingo --version".lineStream_!
          val split = t.head.split(" ")
          val clingoVersion = split(2).split("\\.")(0)
          if (clingoVersion.toInt < 5) {
            logger.info(s"Clingo version >= 5 is required found version ${split(2)} from ${"which clingo".lineStream_!.head}")
          }
          clingo = s"which clingo".lineStream_!.head
          Globals.clingo = clingo
        } catch {
          case _: java.io.IOException =>
            logger.error(s"No clingo not found in PATH")
            System.exit(-1)
        }
    }

    //-------------
    // Global sets:
    //-------------
    Globals.glvalues("specializationDepth") = specializationDepth.toString
    Globals.glvalues("tp-weight") = tpWeight.toString
    Globals.glvalues("fp-weight") = fpWeight.toString
    Globals.glvalues("fn-weight") = fnWeight.toString
    Globals.glvalues("with-inertia") = withInertia.toString
    Globals.glvalues("weight-learning") = weightLearn.toString
    Globals.glvalues("with-ec") = withEventCalculus.toString

    if (entryPath == "None") {
      if (task.nonEmpty) {
        if (tasks.keySet.contains(task)) {
          Trail.showTaskInfo(tasks(task))
          System.exit(0)
        } else {
          logger.error(s"Task $task undefined")
          System.exit(-1)
        }
      } else {
        Trail.showGeneralInfo()
        System.exit(0)
      }
    }

    if (usage.nonEmpty) {
      if (task.nonEmpty) {
        Trail.showTaskInfo(tasks(task))
        System.exit(0)
      } else {
        Trail.showGeneralInfo()
        System.exit(0)
      }
    }

    // Define this here so that all values in Globals.glvalues be already set.
    val globals = new Globals(entryPath.toString)

      // show the run-time options:
      //logger.info(s"\nRunning with options:\n${map.map { case (k, v) => s"$k=$v" }.mkString(" ")}\n")

      def getTargets(input: String) = {
        if (input.contains("|"))
          input.split("\\|")
        else
          Array(input)
      }

    val inps = new RunningOptions(
      entryPath.toString, delta.toString.toDouble, pruningThreshold.toString.toDouble,
      minSeenExmpls.toString.toInt, specializationDepth.toString.toInt, breakTiesThreshold.toString.toDouble,
      repeatFor.toString.toInt, chunkSize.toString.toInt, onlinePruning.toString.toBoolean,
      withPostPruning.toString.toBoolean, getTargets(targetConcept.toString), mintps.toString.toInt,
      tryMoreRules.toString.toBoolean, scoringFun.toString, train.toString, globals, minEvaluatedOn.toString.toInt, shuffleData.toString.toBoolean,
      showRefs.toString.toBoolean, pruneAfter.toString.toInt, tpWeight.toString.toInt, fpWeight.toString.toInt, fnWeight.toString.toInt,
      withInertia.toString.toBoolean, weightLearn.toString.toBoolean,
      adagradDelta.toString.toDouble, adaLearnRate.toString.toDouble, adaRegularization.toString.toDouble,
      adaLossFunction.toString, withEventCalculus.toString.toBoolean, saveTheoryTo.toString, test.toString,
      ruleLearningStrategy.toString, infoGainAtLeast.toString.toDouble, inputTheory.toString,
      removeRules.toString.toBoolean, debug.toString.toBoolean, clingo, findAllOpt.toString.toBoolean,
      perfectFit.toString.toBoolean, data.toString, task, inferenceMode.toString)

    inps
  }

  /**
    * Definitions of basic tasks in TRAIL.
    */
  val infer = "infer"
  val learnrev = "learnrev"
  val inclearn = "inclearn"
  val oled = "oled"
  val woled = "woled"
  val wlearn = "wlearn"

  val tasks = mutable.LinkedHashMap(
    infer -> Task(
      name        = infer,
      description = "Crisp or probabilistic (MAP) inference.",
      usage       = "trail inference <options>"
    ),

    learnrev -> Task(
      name        = learnrev,
      description = "Batch structure learning & theory revision.",
      usage       = "trail learnRevise <options>"
    ),

    inclearn -> Task(
      name        = inclearn,
      description = "Incremental theory learning.",
      usage       = "trail inclearn <options>"
    ),

    oled -> Task(
      name        = oled,
      description = "Run OLED for online structure learning.",
      usage       = "trail oled <options>"
    ),

    woled -> Task(
      name        = woled,
      description = "Run WOLED for online structure + weight learning.",
      usage       = "trail woled <options>"
    ),

    wlearn -> Task(
      name        = wlearn,
      description = "Weight learning for some input theory.",
      usage       = "trail wlearn <options>"
    )
  )

  /**
    * Definitions of command-line arguments.
    */

  val arguments = Vector(

    Argument(
      name      = "--inpath",
      valueType = "String",
      text      = "Path to the BK and mode declarations files.",
      default   = "None",
      relatedTo = List(infer, learnrev, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--train",
      valueType = "String",
      text      = "Training set location. Refer to the manual on more information on how to pass data into TRAIL.",
      default   = "None",
      List(learnrev, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--data",
      valueType = "String",
      text      = "Data source location (used for inference). Refer to the manual on more information on how to pass data into TRAIL.",
      default   = "None",
      List(infer)
    ),

    Argument(
      name      = "--test",
      valueType = "String",
      text      = "Testing set location. Refer to the manual on more information on how to test models with TRAIL.",
      default   = "None",
      List(learnrev, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--clingo",
      valueType = "String",
      text      = "The path to a Clingo binary file.",
      default   = s"${System.getProperty("user.dir")}/dependencies/clingo/build/bin/clingo",
      List(infer, learnrev, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--optall",
      valueType = "Boolean",
      text      = "Return all optimal theories.",
      default   = "false",
      List(learnrev)
    ),

    Argument(
      name      = "--perfect-fit",
      valueType = "Boolean",
      text      = "Switch between perfect/approximate example coverage.",
      /*"If true perfect coverage of the training examples is pursued during learning. NOTE: this is used primarily by learnRevise" +
        " when learning from very few data points, where the standard compression-maximization objective that is used during learning fails to " +
        "learn a theory. On the other hand, --perfect-fit=true will fail (with an 'UNSATISFIABLE PROGRAM' error) even with the slightest noise in the data." +
        "Therefore, although the option applies to the incremental/online learning strategies in TRAIL, which typically deal with larger (and noisy datasets)" +
        " it is rarely useful with these strategies."*/
      default = "false",
      List(learnrev, inclearn, oled, woled)
    ),

    Argument(
      name      = "--debug",
      valueType = "Boolean",
      text      = "Save ASP programs for debugging.",
      default   = "false",
      List(infer, learnrev, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--delta",
      valueType = "Double",
      text      = "Confidence value for Hoeffding tests.", default = "0.05",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--prune",
      valueType = "Double",
      text      = "Precision threshold for acceptable rules.",
      default   = "0.0",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--chunksize",
      valueType = "Int",
      text      = "Mini-batch size. ",
      default   = "1",
      relatedTo = List(infer, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--minseen",
      valueType = "Int",
      text      = "Min. number of examples to evaluate on before breaking ties.",
      default   = "0",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--spdepth",
      valueType = "Int",
      text      = "Specialization depth. All specializations of a rule up to this length are tried simultaneously.",
      default   = "1",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--ties",
      valueType = "Double",
      text      = "Tie-breaking threshold.",
      default   = "0.05",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--repfor",
      valueType = "Int",
      text      = "Re-iterate over the data this-many times.",
      default   = "1",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--onlineprune",
      valueType = "Boolean",
      text      = "Prune rules online.",
      default   = "false",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--postprune",
      valueType = "Boolean",
      text      = "Prune rules after learning terminates.",
      default   = "true",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--weight-learning",
      valueType = "Boolean",
      text      = "Switch weight learning on/off.",
      default   = "false",
      relatedTo = List(inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--ada-delta",
      valueType = "Double",
      text      = "Delta parameter for AdaGrad (weight learning).",
      default   = "1.0",
      relatedTo = List(woled, wlearn)
    ),

    Argument(
      name      = "--ada-learn-rate",
      valueType = "Double",
      text      = "Learning rate for AdaGrad (weight learning).",
      default   = "1.0",
      relatedTo = List(woled, wlearn)
    ),

    Argument(
      name      = "--ada-regularization",
      valueType = "Double",
      text      = "Regularization for AdaGrad (weight learning).",
      default   = "0.001",
      relatedTo = List(woled, wlearn)
    ),

    Argument(
      name      = "--ada-loss-function",
      valueType = "String",
      text      = "Loss function for AdaGrad.",
      default   = "default",
      relatedTo = List(woled, wlearn)
    ),

    Argument(
      name      = "--try-more-rules",
      valueType = "Boolean",
      text      = "Generate more rules than those necessary.",
      default   = "false",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--target",
      valueType = "String",
      text      = "Target concept.",
      /*
      *  This is used in case we need to learn discriminatively for one target concept only. " +
        "NOTE: This might cause problems if used without caution. This command line argument is deprecated " +
        "and it is kept for backwards compatibility only. It is strongly adviced to use exclusively the mode " +
        "declarations to control the target predicates for learning.*/
      default   = "None",
      relatedTo = List(infer, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--scorefun",
      valueType = "String",
      text      = "Scoring function. Values are 'default' (use precision), 'foilgain', 'fscore'.",
      default   = "foilgain",
      relatedTo = List(inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--eval-atleast-on",
      valueType = "Int",
      text      = "Require that output rules have been trained on a min. number of examples.",
      default   = "1000",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--min-pos-covered",
      valueType = "Int",
      text      = "Require that a rule covers a min. number of positives.",
      default   = "0",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--shuffle-data",
      valueType = "Boolean",
      text      = "Shuffle the data (used for experimenting with order effects).",
      default   = "false",
      relatedTo = List(inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--showrefs",
      valueType = "Boolean",
      text      = "Show candidate refinements.",
      default   = "false",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--prune-after",
      valueType = "Int",
      text      = "(DEPRECATED) Minimum number of examples after which a bad rule may be pruned.",
      default   = "10000",
      relatedTo = List(oled, woled)
    ),

    Argument(
      name      = "--data-limit",
      valueType = "Int",
      text      = "Fetch that-many data from a data source to learn from.",
      default   = s"${Double.PositiveInfinity.toInt}",
      relatedTo = List(inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--tps-weight",
      valueType = "Int",
      text      = "Weight on true positive instances.",
      default   = "1",
      relatedTo = List(learnrev, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--fps-weight",
      valueType = "Int",
      text      = "Weight on false positive instances.",
      default   = "1",
      relatedTo = List(learnrev, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--fns-weight",
      valueType = "Int",
      text      = "Weight on false negative instances.",
      default   = "10",
      relatedTo = List(learnrev, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--inertia",
      valueType = "Boolean",
      text      = "Propagate inference to the next mini-batch to simulate inertia in incremental reasoning.",
      default   = "false",
      relatedTo = List(inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--with-ec",
      valueType = "Boolean",
      text      = "Set to false if not learning with the Event Calculus",
      default   = "true",
      relatedTo = List(infer, learnrev, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--show-stats",
      valueType = "Boolean",
      text      = "Print performance stats.",
      default   = "false",
      List(infer, learnrev, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--saveto",
      valueType = "String",
      text      = "Path to a file to write results.",
      default   = "",
      List(infer, learnrev, inclearn, oled, woled, wlearn)
    ),

    //Argument(name      = "--holdout", valueType = "Int", text = "Perform holdout evaluation on a test set every <Int> time points. Omit if --holdout=0", default = "0"),
    //Argument(name      = "--prequential", valueType = "Boolean", text = "If true perform prequential evaluation on every incoming data batch.", default = "true"),
    //Argument(name      = "--selftrain", valueType = "Boolean", text = "If true performs simple self-training from unlabeled data (experimental).", default = "false"),
    //Argument(name      = "--preprune", valueType = "Double", text = "Do not specialize a rule if its score is greater than this threshold.", default = "1.0"),
    //Argument(name      = "--coresnum", valueType = "Int", text = "Number of cores. This is used by the distributed version.", default = "1"),
    //Argument(name      = "--compress-new-rules", valueType = "Boolean", text = "If true new rules originating from bottom clauses that have already been generated previously are dropped.", default = "true"),

    Argument(
      name      = "--rule-learning-strategy",
      valueType = "String",
      text      = "Rule construction strategies for OLED/WOLED. " +
        "Values: 'hoeffding', 'tr'", default = "hoeffding",
      List(oled, woled)
    ),

    Argument(
      name      = "--infogain",
      valueType = "Double",
      text      = "Min. info gain of a child over its parent rule.",
      default   = "0.001",
      List(oled, woled)
    ),

    Argument(
      name      = "--input-theory",
      valueType = "String",
      text      = "Path to file containing an input theory.",
      default   = "",
      List(infer, learnrev, inclearn, oled, woled, wlearn)
    ),

    Argument(
      name      = "--remove-rules",
      valueType = "Boolean",
      text      = "Remove unnecessary/low-quality rules during theory revision.",
      default   = "false",
      List(learnrev, inclearn, oled, woled)
    ),

    Argument(
      name      = "--mode",
      valueType = "String",
      text      = "Options for inference. Values are 'show', 'eval', 'explain'",
      default   = "eval",
      List(infer)
    ),

    Argument(
      name      = "--sspace",
      valueType = "String",
      text      = "Options for search space construction. Values are 'mdriven', 'ddriven', 'enum_<Int>'",
      default   = "mdriven",
      List(learnrev, inclearn, oled, woled)
    ),

  )

  def splitString(s: String, l: Int, chunks: Vector[String]): Vector[String] = {
    s.length > l match {
      case true =>
        val first = s.splitAt(l)
        splitString(first._2, l, chunks :+ first._1)
      case _ => chunks :+ s
    }
  }

  def helpMesg = {
    val msg = (x: Argument) => s"${x.name}=<${x.valueType}> | default=<${x.default}>"
    val maxLength = arguments.map(x => msg(x).length).max
    val thisLength = (x: Argument) => msg(x).length
    val message = (x: Argument) => s"  ${msg(x)} ${" " * (maxLength - thisLength(x))} : ${x.text}"
    //val message = (x: Argument) => s"  ${msg(x)} ${" " * (maxLength - thisLength(x))} : ${splitString(x.text, 30, Vector[String]())}"
    (List("\nOLED options:\n") ++ arguments.map(x => message(x))).mkString("\n")
  }

  /*Checks if mandatory arguments are in place. Returns (msg, false) if they are not else ("", true)*/
  def argsOk(args: Array[String]): (Boolean, String) = {
    if (args.isEmpty) {
      (false, "Missing options. Run with --help.")
    } else if (args.exists(x => x.contains("--help"))) {
      (false, helpMesg)
    } else if (!args.exists(x => x.contains("--inpath"))) {
      (false, "A mandatory option is missing (e.g. path to bk/mode declarations files or the name of a database with training examples)." +
        " Re-run with --help to see options")
    } else {
      (true, "")
    }
  }

  /*def checkData(dataInput: String, collection: String, trainOrTest: String) = {
    val msg = if (trainOrTest == "train") "train" else "test"
    // Check if it's a file
    val fileExists = new java.io.File(dataInput).exists
    if (!fileExists) {
      // check if it's a db
      val dbok = checkDB(dataInput, collection)
      if (!dbok) {
        logger.error(s"Running with --$msg=$dataInput but that's neither a database nor a file")
        System.exit(-1)
      }
    }
  }*/

  /* If this returns false either the db does not exist or it is empty. */
  /*def checkDB(dbName: String, colName: String) = {
    val mongoClient = MongoClient()
    val exists = mongoClient.databaseNames().toSet.contains(dbName)
    if (!exists) {
      logger.error(s"Database $dbName does not exist")
      false
    } else {
      val collection = mongoClient(dbName)(colName)
      val nonEmpty = collection.nonEmpty
      mongoClient.close()
      if (!nonEmpty) {
        logger.error(s"Database $dbName is empty.")
      }
      nonEmpty
    }
  }*/

}

case class Argument(name: String, valueType: String, text: String, default: String, relatedTo: List[String])
case class Task(name: String, description: String, usage: String)

class RunningOptions(
    val entryPath: String,
    val delta: Double,
    val pruneThreshold: Double,
    val minSeenExmpls: Int,
    val specializationDepth: Int,
    val breakTiesThreshold: Double,
    val repeatFor: Int,
    val chunkSize: Int,
    val onlinePruning: Boolean,
    val withPostPruning: Boolean,
    val targetConcepts: Array[String],
    val minTpsRequired: Int,
    val tryMoreRules: Boolean,
    val scoringFun: String,
    val train: String,
    val globals: Globals,
    val minEvalOn: Int,
    val shuffleData: Boolean,
    val showRefs: Boolean,
    val pruneAfter: Int,
    val tpWeight: Int,
    val fpWeight: Int,
    val fnWeight: Int,
    val withInertia: Boolean,
    val weightLean: Boolean,
    val adaGradDelta: Double,
    val adaLearnRate: Double,
    val adaRegularization: Double,
    val adaLossFunction: String,
    val withEventCalculs: Boolean,
    val saveTheoryTo: String,
    val test: String,
    val ruleLearningStrategy: String,
    val infoGainAtLeast: Double,
    val inputTheory: String,
    val removeRules: Boolean,
    val debug: Boolean,
    val clingo: String,
    val findAllOpt: Boolean,
    val perfectFit: Boolean,
    val data: String,
    val task: String,
    val mode: String)

