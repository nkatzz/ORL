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

package oled.learning.structure

import java.io.{BufferedWriter, File, FileWriter}
import java.util.UUID

import com.mongodb.{BasicDBList, BasicDBObject}
import com.mongodb.casbah.{MongoClient, MongoCollection}
import com.mongodb.casbah.commons.MongoDBObject
import com.typesafe.scalalogging.LazyLogging
import com.mongodb.casbah.Imports._

import scala.sys.process._
import oled.app.runutils.Globals
import oled.datahandling.Example
import oled.inference.ASPSolver
import oled.learning.Types.Theory
import oled.logic.parsers.ClausalLogicParser
import oled.logic.{Clause, Constant, Literal, LogicalExpression, ModeAtom}

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

/**
  * Created by nkatz at 14/12/19
  */

/**
  * THIS NEEDS CLEAN UP (PROBABLY ALL OF THE STUFF HERE SHOULD BE THROWN AWAY AND REWRITE THE BASIC FUNCTIONALITY).
  *
  */

object OldStructureLearningFunctions extends ASPResultsParser with LazyLogging {

  def generateNewRules(topTheory: List[Clause], e: Example, initorterm: String, globals: Globals) = {
    val bcs = generateNewBottomClauses(topTheory, e, initorterm, globals)
    bcs map { x =>
      val c = Clause(head = x.head, body = List())
      c.addToSupport(x)
      c
    }
  }

  def generateNewBottomClauses(topTheory: List[Clause], e: Example, initorterm: String, globals: Globals) = {

    val terminatedOnly = if (initorterm == "terminatedAt") true else false
    val specialBKfile = if (initorterm == "initiatedAt") globals.BK_INITIATED_ONLY else globals.BK_TERMINATED_ONLY

      def toMapASP(e: Example) = Map("annotation" -> e.queryAtoms.map(x => s"example($x)."), "narrative" -> e.observations.map(x => x + "."))

    val (_, varKernel) =
      generateKernel1(toMapASP(e), learningTerminatedOnly = terminatedOnly, bkFile = specialBKfile, globals = globals)
    val bottomTheory = topTheory flatMap (x => x.supportSet)
    val goodKernelRules = varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))
    goodKernelRules
  }

  def generateKernel1(examples: Map[String, List[String]], fromWeakExmpl: Boolean = false,
      learningTerminatedOnly: Boolean = false, bkFile: String, globals: Globals) = {

    val infile = getTempFile("example", ".lp")
    val f = (x: String) => if (x.endsWith(".")) x else s"$x."
    val interpretation = examples("annotation").map(x => s"${f(x)}") ++ examples("narrative").map(x => s"${f(x)}")
    writeToFile(infile, "overwrite") { p => interpretation.foreach(p.println) }
    var (kernel, varKernel) =
      runXhail(fromFile                 = infile.getAbsolutePath,
        kernelSetOnly = true,
               fromWeakExmpl            = fromWeakExmpl,
        learningTerminatedAtOnly = learningTerminatedOnly,
        bkFile = bkFile,
        globals = globals)

    infile.delete()
    (kernel, varKernel)
  }

  def runXhail(
      fromFile: String = "",
      fromDB: String = "",
      inputDirectory: String = "",
      kernelSetOnly: Boolean = false,
      learningTerminatedAtOnly: Boolean = false,
      //keepAbducedPreds: String = "all",
      fromWeakExmpl: Boolean = false,
      bkFile: String,
      globals: Globals): (List[Clause], List[Clause]) = {

    val matches = (p: Regex, str: String) => p.pattern.matcher(str).matches
    val examples: Map[String, List[String]] = fromFile match {
      case "" =>
        fromDB match {
          case "" => throw new RuntimeException("Provide a file or a mongo DB with the training examples.")
          case _ => getAllExamples(fromDB, "examples")
        }
      case _ =>
        val all =
          Source.fromFile(fromFile).getLines.toList.map(x => x.replaceAll("\\s", "")).filter(line => !matches("""""".r, line))
        val annotation = all.filter { x => x.contains("example(") }
        val narrative = all.filter { x => !x.contains("example(") }
        Map("annotation" -> annotation, "narrative" -> narrative)
    }

    val aspFile: File = getTempFile("aspinput", ".lp")
    val abdModels: List[AnswerSet] =
      abduce("modehs", examples                 = examples, learningTerminatedAtOnly = learningTerminatedAtOnly, fromWeakExmpl = fromWeakExmpl, bkFile = bkFile, globals = globals)
    //if (abdModel != Nil) logger.info("Created Delta set")
    //println(abdModels)
    val (kernel, varKernel) = abdModels match {
      case Nil => (List[Clause](), List[Clause]())
      case _ =>
        if (!Globals.glvalues("iter-deepening").toBoolean) {
          //val abduced = if(learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms
          val abduced =
            /*
          if(!oledLearningInitWithInertia) {
            if(learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms
          } else {
            //if(learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms
            if(learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms.filter(_.contains("initiatedAt"))
          }
          */
            if (learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms
          //if(learningTerminatedAtOnly) abdModels.head.atoms.filter(_.contains("terminatedAt")) else abdModels.head.atoms.filter(_.contains("initiatedAt"))
          generateKernel(abduced, examples = examples, aspInputFile = aspFile, bkFile = bkFile, globals = globals)
        } else {
          return (List.empty[Clause], List.empty[Clause])
          //return iterativeSearch(abdModels, examples, kernelSetOnly, bkFile, globals) // this is used from ILED to find a kernel with iterative search
        }
    }
    //if (!kernelSetOnly) findHypothesis(varKernel, examples = examples, globals=globals)
    (kernel, varKernel)
  }

  def getAllExamples(db: String, collection: String,
      alternativeAspFile: String = ""): Map[String, List[String]] = {

    // Get all examples from a DB and write them to ASP input files.
    // Returns true if the current DB has supervision, else false
    val mongoClient = MongoClient()
    val col = mongoClient(db)(collection)
    //val col = MongoClient()(db)(collection)
    val out = examplestoASP("all", "all", col, alternativeAspFile)
    mongoClient.close()
    out
  }

  def examplestoASP(
      field: String,
      fieldValue: Any,
      collection: MongoCollection,
      alternativeAspFile: String = ""): Map[String, List[String]] = {

    var annotation = new ListBuffer[String]()
    var narrative = new ListBuffer[String]()
    field match {
      case "all" =>
        for (x <- collection.find().sort(MongoDBObject("time" -> 1))) {
          annotation = annotation ++ x.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(x => s"example($x).")
          narrative = narrative ++ x.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(x => s"$x.")
        }

      case "\\s" => throw new RuntimeException("Which example do you want?")
      case _ => fieldValue match {
        case "\\s" => throw new RuntimeException("Which example do you want?")
        case "all" => throw new RuntimeException("Execution should not have reached this code")
        case _ =>
          val query = MongoDBObject(field -> fieldValue)
          try {
            val target = collection.findOne(query).get
            annotation = annotation ++ target.asInstanceOf[BasicDBObject].get("annotation").asInstanceOf[BasicDBList].toList.map(x => s"example($x).");
            narrative = narrative ++ target.asInstanceOf[BasicDBObject].get("narrative").asInstanceOf[BasicDBList].toList.map(x => s"$x.");
            //write((annotation, narrative))
          } catch {
            case e: NoSuchElementException =>
              println(s"The example with \'field -> value\' : \'$field -> $fieldValue\' does not exist")

            //System.exit(-1)
          }

      }
    }
    //return if (annotation.length > 0) true else false
    return Map("annotation" -> annotation.toList, "narrative" -> narrative.toList)
  }

  def abduce(
      abducibles: Any,
      numberOfModels: Int = 1000,
      useMatchModesProgram: Boolean = true,
      examples: Map[String, List[String]],
      learningTerminatedAtOnly: Boolean = false,
      fromWeakExmpl: Boolean = false,
      bkFile: String, globals: Globals): List[AnswerSet] = {

    val aspFile: File = getTempFile("aspinput", ".lp", "", deleteOnExit = true)

    val modeDeclarations = globals.MODEHS ++ globals.MODEBS
    val varbedExmplPatterns = globals.EXAMPLE_PATTERNS

      def getASPinput() = {
        globals.MODEHS match {
          case Nil => throw new RuntimeException("No Mode Declarations found.")
          case _ =>
            val varbedMHAtoms = globals.MODEHS map (x => x.varbed)
            val generate: List[String] = varbedMHAtoms.map(
              x => s"{${x.tostring}} :- " + x.typePreds.mkString(",") + ".")
            // Generate minimize statement
            val minimize = Globals.glvalues("iter-deepening") match {
              case "false" =>
                if (Globals.glvalues("perfect-fit").toBoolean) {
                  "\n#minimize{\n" + (varbedMHAtoms map (
                    x => "1," + (x.variables(modeDeclarations) map (y => y.tostring)).mkString(",") + s":${x.tostring}")).mkString(";\n") + "\n}."
                } else {
                  val f = (x: Literal) => "1," + (x.variables(modeDeclarations) map (y => y.tostring)).mkString(",")
                  val ff = varbedExmplPatterns.map(x =>
                    s"${f(x)},posNotCovered(${x.tostring}):example(${x.tostring})," +
                      s" not ${x.tostring};\n${f(x)},negsCovered(${x.tostring}):${x.tostring}," +
                      s" not example(${x.tostring})").mkString(";")
                  "\n#minimize{\n" + (varbedMHAtoms map (
                    x => "1," + (x.variables(modeDeclarations) map (y => y.tostring)).mkString(",") + s":${x.tostring}")).mkString(";\n") + s";\n$ff\n}."
                }

              // If we want iterative deepening then we drop the minimize statements
              // Also to use iterative deepening we'll have to pass the required generateAtLeast
              // and generateAtMost parameters.
              case _ => ""
            }

            val coverageConstr: List[String] =
              if (Globals.glvalues("perfect-fit").toBoolean) {
                getCoverageDirectives(learningTerminatedAtOnly, globals = globals)
              } else {
                val z = varbedExmplPatterns.map { x =>
                  s"\nposNotCovered(${x.tostring}) :- example(${x.tostring}), not ${x.tostring}." +
                    s"\nnegsCovered(${x.tostring}) :- ${x.tostring}, not example(${x.tostring})."
                }.mkString("\n")
                List(z)
              }

            val modeMatchingProgram =
              if (useMatchModesProgram)
                matchModesProgram(globals.MODEHS.map(x => x.varbed))
              else List()

            toASPprogram(
              program = List(s"#include " +
                "\"" + bkFile + "\"" + ".\n\n") ++
                examples("annotation") ++ List("\n\n") ++
                examples("narrative") ++ List("\n\n") ++
                coverageConstr ++ generate ++ List(minimize),
              extra   = modeMatchingProgram,
              writeTo = aspFile.getCanonicalPath)

        }
      }

    abducibles match {
      case "modehs" => getASPinput()
      /* This is for the case where abducibles are explicitly given.
       *
       * @todo: Implement this logic
       *
       * */
      case _: List[Any] => throw new RuntimeException("This logic has not been implemented yet.")
      case _ => throw new RuntimeException("You need to specify the abducible predicates.")
    }

    solveASP(Globals.ABDUCTION, aspFile.getAbsolutePath)
  }

  def generateKernel(
      abdModel: List[String],
      alternativePath: String = "",
      examples: Map[String, List[String]],
      aspInputFile: java.io.File,
      bkFile: String,
      globals: Globals): (List[Clause], List[Clause]) = {

      //val bkFile = globals.BK_WHOLE_EC

      def replaceQuotedVars(x: String) = {
        val varPattern = "\"([A-Z][A-Za-z0-9_])*\"".r
        val matches = varPattern.findAllIn(x).toList
        val vars = matches.map(x => x.replaceAll("\"", ""))
        val zipped = matches zip vars
        zipped.foldLeft(x) { (p, q) =>
          val quotedVar = q._1
          val strippedVar = q._2
          p.replaceAll(quotedVar, strippedVar)
        }
      }

      def groundBodyModesWithInTerms(interms: List[LogicalExpression]): List[(List[String], ModeAtom)] = {

        val filterout = (x: String, y: Regex, z: List[String]) => z.filter(e => !y.findAllIn(x).toList.map(q => replaceQuotedVars(q)).exists(e.contains(_)))

        val p: List[String] =
          for (
            x <- interms;
            pred = x.asInstanceOf[Constant]._type;
            arg = x.asInstanceOf[Constant].name
          ) yield s"$pred($arg)."

        val mapping = (globals.MODEBS map (x => (globals.MODEBS.indexOf(x), x))).toMap
        val groundModes =
          for (
            x <- globals.MODEBS;
            varb = x.varbed;
            quoted = x.varbed.tostringQuote;
            quoatedNoNeg = x.varbed.nonNegated.tostringQuote;
            filtered = filterout(quoted, "\"([A-Za-z0-9_])*\"".r, varb.typePreds)
          ) yield // surround with triple quotes to allow double quotes in the string
          //s"""ground(${globals.MODEBS.indexOf(x)},$quoatedNoNeg) :- ${filterout(quoted, "\"([A-Za-z0-9_])*\"".r, varb.typePreds).mkString(",")}."""
          if (filtered.nonEmpty) {
            s"""ground(${globals.MODEBS.indexOf(x)},$quoatedNoNeg) :- ${filtered.mkString(",")}."""
          } else {
            s"""ground(${globals.MODEBS.indexOf(x)},$quoatedNoNeg) :- #true."""
          }

        toASPprogram(
          program = p ++ groundModes ++ List("\n\n#show ground/2."),
          writeTo = aspInputFile.getCanonicalPath)

        val q = solveASP("getQueries", aspInputFile.getCanonicalPath)
        /*
      val result =
        (for (x <- q.head.atoms;
              tolit = Literal.toLiteral(x);
              mode = mapping(tolit.terms.head.name.toInt);
              groundTerm = tolit.terms(1))
          yield (mode,groundTerm)).groupBy(_._1).mapValues(p => p map (q => q._2)).map(z => (z._2.map(k=>k.tostring),z._1)).toList
      */

        //println(q)

        val result =
          (for (
            x <- q.head.atoms;
            tolit = Literal.parse(x);
            mode = mapping(tolit.terms.head.name.toInt);
            groundTerm = tolit.terms(1)
          ) yield (mode, groundTerm)).groupBy(_._1).toList map { case (k, v) => (for ((_, m) <- v) yield m.tostring, k) }

        result
      }

    //println(abdModel)

    val modeDecl = globals.MODEHS ++ globals.MODEBS

    // Map[Modes.ModeAtom, List[(Modes.ModeAtom, Expression)]]
    val abducedAtoms: List[Literal] = for (
      x <- abdModel;
      tolit = Literal.parse(x);
      (atom, modeAtom) = try {
        (tolit.terms(1), globals.MODEHS(tolit.terms.head.asInstanceOf[Constant].name.toInt - 1))
      } catch {
        case e: java.lang.ClassCastException => (tolit, tolit.matchingMode(modeDecl))
      }
    ) yield Literal.toLiteral2(atom.asInstanceOf[Literal], modeAtom.asInstanceOf[ModeAtom])

    var kernelSet = new ListBuffer[Clause]()
    if (Globals.glvalues("iter-deepening").toBoolean) logger.info(abducedAtoms.map(_.tostring).mkString(" "))
    for (x <- abducedAtoms) {
      var body = new ListBuffer[Literal]()
      val (_interms, _, _) = x.placeMarkers
      val interms = _interms.to[ListBuffer]
      var solution = List[AnswerSet]()
      for (i <- 0 to Globals.glvalues("variableDepth").toInt) {

        val queries = groundBodyModesWithInTerms(interms.toList)

        val deduce =
          (for (
            (queryList, mAtom) <- queries;
            show = queryList map (x => replaceQuotedVars(x)) map (x =>
              //"\n#show " + "ifTrue("+Core.modebs.indexOf(mAtom)+","+x+")" + ":" + (if (!mAtom.isNAF) x else "not "+x) + ".\n")
              s"\n#show ifTrue(${globals.MODEBS.indexOf(mAtom)},$x) : ${if (!mAtom.isNAF) x else "not " + x}, ${Literal.types(x, mAtom, globals)}.")
          ) yield show).flatten

        val program = abdModel.map(x => x + ".")

        toASPprogram(program =
          examples("annotation") ++
            examples("narrative") ++
            program ++
            List(s"\n#include " + "\"" + bkFile + "\".") ++
            List("\n#show.\n") ++ deduce, writeTo = aspInputFile.getCanonicalPath)

        solution = solveASP("deduction", aspInputFile.getCanonicalPath)
        if (solution.nonEmpty) {
          val f = (x: (LogicalExpression, LogicalExpression)) => {
            val mode = globals.MODEBS(x._1.asInstanceOf[Constant].name.toInt)
            val lit =
              if (mode.isNAF) {
                Literal.toLiteral2(x._2.asInstanceOf[Literal]).negated
              } else {
                Literal.toLiteral2(x._2.asInstanceOf[Literal]).nonNegated
              }
            Literal.toLiteral2(lit, mode)
          }

          val _b = solution.head.atoms.asInstanceOf[List[String]].distinct map (
            x => Literal.parse(x)) map (
              x => (x.terms.head, x.terms(1))) map (
                //x => Literal.toLiteral2(x._2.asInstanceOf[Literal], Core.modebs(x._1.asInstanceOf[Constant].name.toInt))
                x => f(x))

          ///*
          // just to make woled work a bit faster, since you cannot cut these atoms from lomrf
          // (such redundant atoms can be automatically pruned with clingo, but you cannot do this with lomrf).
          // I need to fix this
          val b = _b.filter { x =>
            x.predSymbol != "close" || (x.predSymbol == "close" && x.terms(0).name != x.terms(1).name)
          }
          //*/

          //val b = _b

          for (k <- b) {
            //if (!body.contains(k)) body ++= b
            if (!body.exists(x => x.tostring == k.tostring)) body += k
            val (_, outTerms, _) = k.placeMarkers
            interms ++= outTerms
          }

        }
      }
      if (solution.nonEmpty) {
        val kernelClause = Clause(x, body.toList.distinct)
        kernelSet += kernelClause
      }

    }
    val _varKernel = kernelSet.map(x => x.varbed)

    // Remove redundant comparison literals for the variabilized kernel to simplify things...
    val varKernel = _varKernel.map(x => simplifyRule(x, globals))

    //val varKernel = _varKernel

    val vlength = varKernel.length
    val compressed = if (Globals.glvalues("compressKernels").toBoolean) compressTheory(varKernel.toList) else varKernel.toList
    compressed foreach (x => x.isBottomRule = true)
    val clength = compressed.length
    val nonEmptyVarKernel = compressed.filter(x => x.body.nonEmpty)
    //val nonEmptyKernel = kernelSet.filter(x => x.body.nonEmpty)

    if (nonEmptyVarKernel.nonEmpty) {
      //logger.info(s"Created Kernel set:\n${nonEmptyVarKernel.map(x => x.tostring).mkString("\n")}")

      logger.info(s"Created Kernel set")

      logger.debug("\n------------------------------------------------------------------------------------\n" +
        s"Kernel Set (Ground---Variabilized($vlength clauses)---Compressed($clength clauses)):" +
        "\n------------------------------------------------------------------------------------\n" +
        showTheory(kernelSet.toList) + "\n\n" + showTheory(varKernel.toList) + "\n\n" + showTheory(compressed.toList))
      //println(Theory(kernelSet.toList).tostring)
    }

    (kernelSet.toList, compressed)
  }

  val showTheory = (x: List[Clause]) => x.map { x => x.tostring }.mkString("\n")

  def compressTheory(kernel: List[Clause]): List[Clause] = {
    val compressed = new ListBuffer[Clause]
    val included = (c: Clause) => compressed.toList.exists(x => x.thetaSubsumes(c) && c.thetaSubsumes(x))
    for (c <- kernel) {
      //val others = kernel.filter(x => x != c)
      if (!included(c)) compressed += c
    }
    compressed.toList
  }

  /*
  * Simplifies a rule by removing redundant comparison predicates from its body.
  * */
  def simplifyRule(c: Clause, gl: Globals) = {

    val (nonComparisonPreds, comparisonPreds) = c.body.foldLeft(Set[Literal](), Set[Literal]()) { (accum, lit) =>
      if (gl.comparisonPredicates.contains(lit.modeAtom)) (accum._1, accum._2 + lit) else (accum._1 + lit, accum._2)
    }

    val grouped = comparisonPreds.groupBy(x => x.modeAtom)

    val simplified = grouped.map {
      case (modeAtom, literals) =>
        if (modeAtom.compRelation == "lessThan") {
          literals.toList.minBy(_.getComparisonTerm.name.toInt)
        } else if (modeAtom.compRelation == "greaterThan") {
          literals.toList.maxBy(_.getComparisonTerm.name.toInt)
        } else {
          throw new RuntimeException(s"Don't know what to do with this comparison relation: ${modeAtom.compRelation}")
        }
    }.toSet

    val newTerms = nonComparisonPreds.toList ++ simplified.toList

    val cc = Clause(head = c.head, body = newTerms, uuid = c.uuid)
    // Just to be on the safe side...
    cc.supportSet = c.supportSet
    cc.parentClause = c.parentClause
    //cc.countsPerNode = c.countsPerNode
    cc.weight = c.weight
    cc.subGradient = c.subGradient
    //cc.w_pos = c.w_pos
    //cc.totalTPs = c.totalTPs
    //cc.totalFPs = c.totalFPs
    //cc.totalFNs = c.totalFNs
    //cc.totalSeenExmpls = c.totalSeenExmpls
    cc.tps = c.tps
    cc.fps = c.fps
    cc.fns = c.fns
    cc.refinements = c.refinements
    cc.seenExmplsNum = c.seenExmplsNum
    cc.previousMeanDiffCount = c.previousMeanDiffCount
    cc.previousMeanScoreCount = c.previousMeanScoreCount
    cc.previousMeanDiff = c.previousMeanDiff
    cc.previousScore = c.previousScore

    cc

  }

  object AnswerSet {

    def UNSAT = new AnswerSet(List(Globals.UNSAT))
    def empty = new AnswerSet(List[String]())
  }

  case class AnswerSet(atoms: List[String]) {

    val isEmpty = atoms == List()

  }

  def solveASP(task: String, aspFile: String, fromWeakExmpl: Boolean = false): List[AnswerSet] = {

    val solveMode =
      if (task == Globals.ABDUCTION && Globals.glvalues("iter-deepening").toBoolean) {
        s"${Globals.glvalues("iterations")}"
      } else {
        "all"
      }

    val with_atom_undefiend = "-Wno-atom-undefined"
    val cores = Runtime.getRuntime.availableProcessors
    val aspCores = s"-t$cores"
    val mode = if (List("all", "optN").contains(solveMode)) "0" else ""
    //val command = Seq("clingo", aspFile, mode, with_atom_undefiend, aspCores, " > ", outFile.getCanonicalPath)

    val command = Seq("clingo", aspFile, mode, with_atom_undefiend, aspCores)

    val result = command.mkString(" ").lineStream_!
    val results = result.toList

    val status = {
      val statusLine = results.filter(x => x.contains("SATISFIABLE") || x.contains("UNSATISFIABLE") || x.contains("OPTIMUM FOUND"))
      if (statusLine.isEmpty) throw new RuntimeException(s"No STATUS returned from Clingo.")
      // extract the actual string literal (SATISFIABLE, UNSATISFIABLE or OPTIMUM FOUND)
      statusLine.head.replaceAll("\\s", "")
    }

    if (status == Globals.UNSAT) {
      task match {
        case Globals.CHECKSAT => return List(AnswerSet.UNSAT)
        case _ =>
          task match {
            // we need this in order to remove inconsistent weak support rules
            case Globals.FIND_ALL_REFMS => return List(AnswerSet.UNSAT)
            // searching alternative abductive explanations with iterative search
            case Globals.SEARCH_MODELS => return List(AnswerSet.UNSAT)
            case Globals.INFERENCE => return List(AnswerSet.UNSAT)
            case Globals.SCORE_RULES => return List(AnswerSet.UNSAT)
            case Globals.GROW_NEW_RULE_TEST => return List(AnswerSet.UNSAT)
            case (Globals.ABDUCTION | Globals.ILED) =>
              // some times a kernel cannot be created from garbage (weak) data points
              // but we don't want a failure in this case. Same holds when generalizing
              // a kernel from a weak data point, to gen a new rule, but end up in an
              // UNSAT program. We don't want a crash in this case either, we simply want
              // to quit learning from the particular example and move on.
              if (fromWeakExmpl) {
                if (task == Globals.ILED) logger.info("Failed to learn something from that...")
                return Nil
              } else {
                /* Perhaps there's no need to crash because the solver get stuck with something... Learning sound stuff in a past no one wants to return to */
                logger.error(s"Task: $task -- Abduction failed (UNSATISFIABLE program)")
                return Nil
                /*
                logger.error(s"\nTask: $task -- Ended up with an UNSATISFIABLE program")
                val program = Source.fromFile(aspFile).getLines.toList.mkString("\n")
                throw new RuntimeException(s"\nTask: $task -- Ended up with an UNSATISFIABLE program:\n$program")
                */
              }
            case _ =>
              logger.error(s"Task: $task -- Abduction failed (UNSATISFIABLE program)")
              return Nil
            /*
            logger.error(s"\nTask: $task -- Ended up with an UNSATISFIABLE program")
            val program = Source.fromFile(aspFile).getLines.toList.mkString("\n")
            throw new RuntimeException(s"\nTask: $task -- Ended up with an UNSATISFIABLE program:\n$program")
            */
          }
      }
    }

    // get the models

    val processLine = (x: String) => parseAll(aspResult, x.replaceAll("\\s", "")) match {
      case Success(result, _) => result
      case f => None
    }

    val _models = results.map(x => processLine(x)).filter(z => z != None).filter(p => p.asInstanceOf[List[String]].nonEmpty).reverse
    //val models = _models filter (x => x.replaceAll("\\s", "") != "")

    //=========================================================================
    // This is a quick fix to get the result for abduction when
    // perfect-fit=false. In this case numerous models are
    // returned, and often the empty one is the optimal (smallest).
    // But the empty one will be filtered out by the code val models = _models
    // and we'll end up doing extra stuff with other models for no real reason
    // I'm just adding this as a quick & dirty fix to make sure that nothing
    // else breaks.
    //=========================================================================
    //-------------------------------------------------------------------------
    if (_models.isEmpty) return Nil
    if (task == Globals.ABDUCTION && _models.head == "") return Nil
    //-------------------------------------------------------------------------

    //outFile.delete() // it's deleted on exit but it's better to get rid of them as soon as we're done with them.

    if (_models.isEmpty) Nil
    else _models map (x => AnswerSet(x.asInstanceOf[List[String]]))
  }

  def getCoverageDirectives(
      learningTerminatedAtOnly: Boolean = false,
      withCWA: String = Globals.glvalues("cwa"),
      checkConsistencyOnly: Boolean = false, globals: Globals): List[String] = {

    val varbedExmplPatterns = globals.EXAMPLE_PATTERNS_AS_STRINGS
    varbedExmplPatterns.flatMap(x =>
      Globals.glvalues("cwa") match {
        // CWA on the examples:
        case "true" =>
          learningTerminatedAtOnly match {
            case true => List(s":- example($x), not $x.", s":- $x, not example($x).", s"$x :- example($x).")
            case _ =>
              if (!checkConsistencyOnly) List(s":- example($x), not $x.", s":- $x, not example($x).")
              else List(s":- $x, not example($x).")

          }
        // No CWA on the examples, agnostic with missing examples, explicit negatives:
        case _ =>
          //List(s":- example($x), not $x.", s":- $x, negExample($x).", s"$x :- example($x).")
          List(s":- example($x), not $x.", s":- $x, negExample($x).")
        /*
        learningTerminatedAtOnly match {
           case true => List(s":- example($x), not $x.", s":- $x, negExample($x).", s"$x :- example($x).")
           case _ =>
             if(!checkConsistencyOnly) List(s":- example($x), not $x.", s":- $x, not example($x).")
             else List(s":- $x, not example($x).")

        }
        */
      })
  }

  def getTempFile(prefix: String, suffix: String,
      directory: String = "",
      deleteOnExit: Boolean = true): File = {

    var file: java.io.File = new java.io.File("")
    directory match {
      case "" => file = java.io.File.createTempFile(s"$prefix-${System.currentTimeMillis()}-${UUID.randomUUID.toString}", suffix)
      case _ => file = java.io.File.createTempFile(s"$prefix-${System.currentTimeMillis()}-${UUID.randomUUID.toString}", suffix, new java.io.File(directory))
    }
    if (deleteOnExit) file.deleteOnExit()
    file
  }

  /**
    * This generates a helper ASP program to extract the mode declaration atoms (if any) that match
    * each atom in an answer set returned by the solver. This helps to process the atoms and populate
    * the objects the are constructed from them as their internal representations. In practice this
    * program computes theta-subsumption between literals.
    *
    * @example This is a (slightly adapted) example from the E.coli case study from:
    *
    * Ray, O. (2009). Nonmonotonic abductive inductive learning. Journal of Applied Logic, 7(3), 329-340.
    *
    * %% Given Mode declarations:
    * -----------------------------------------------
    * modeh(happens(use(#sugar),+time)).
    * modeh(happens(add(#sugar),+time)).
    * modeb(holdsAt(available(#sugar),+time)).
    * modeb(not_holdsAt(available(#sugar),+time)).
    * -----------------------------------------------
    * %% Generate the following program:
    * ----------------------------------------------------------------------------------------------------------
    * mode(1,happens(use(X),Y)) :- sugar(X),time(Y). %% one atom for each mode, counting them with the 1st arg.
    * mode(2,happens(add(X),Y)) :- sugar(X),time(Y).
    * mode(3,holdsAt(available(X),Y)) :- sugar(X),time(Y).
    * mode(4,not_holdsAt(available(X),Y)) :- sugar(X),time(Y).
    *
    * modeCounter(1..4).
    *
    * matchesMode(ModeCounter,Atom,Mode) :-
    *     mode(ModeCounter,Atom), mode(ModeCounter,Mode), true(Atom), Atom = Mode.
    *
    * %% Add one such rule for each predicate (mode atom) you want to query. The purpose is to
    * %% is to generate matchesMode/3 instances only for atoms that are included in an
    * %% answer set (i.e. true atoms), in order to avoid huge amounts of irrelevant info.
    *
    * true(happens(use(X),Y)) :- happens(use(X),Y).
    * true(happens(add(X),Y)) :- happens(add(X),Y).
    * true(holdsAt(available(X),Y)) :- holdsAt(available(X),Y).
    * true(holdsAt(not_available(X),Y)) :- holdsAt(not_available(X),Y).
    *
    * #hide.
    * #show matchesMode/3.
    * ---------------------------------------------------------------------------------------------------------
    *
    * An atom 'matchesMode(m,atom,_)' in an answer set of this program is interpreted as a true atom
    * that matches with mode atom 'm'.
    *
    *
    */
  def matchModesProgram(queryModePreds: List[Literal]): List[String] = {
    val modeDecl: List[String] = for (
      x <- queryModePreds;
      y <- List.range(1, queryModePreds.length + 1) zip queryModePreds
    ) yield "mode(" + y._1 + "," + x.tostring + "," + y._2.tostring + ") :- " + x.typePreds.mkString(",") + "."
    val modeCount: String = "modeCounter(1.." + queryModePreds.length + ")."
    val clause = """matchesMode(ModeCounter,Atom,Mode) :-
       mode(ModeCounter,Atom, Mode), true(Atom), Atom = Mode."""
    val trues: List[String] = for (x <- queryModePreds) yield "true(" + x.tostring + ")" + " :- " + x.tostring + "."
    val program = modeDecl ++ List(modeCount) ++ List(clause) ++ trues ++ List("\n#show matchesMode/3.")
    //program.foreach(println)
    program
  }

  def clearFile(file: String): Unit = {
    val writer = new java.io.PrintWriter(new FileWriter(new java.io.File(file), false))
    writer.write("")
    writer.close()
  }

  def writeToFile(f: java.io.File, howTowrite: String)(op: java.io.PrintWriter => Unit) {
    // Write an iterable to file. Usage:
    //writeToFile(new File("example.txt")) { p => data.foreach(p.println) }
    val p = howTowrite match {
      case "append" => new java.io.PrintWriter(new FileWriter(f, true))
      case "overwrite" => new java.io.PrintWriter(new FileWriter(f, false))
      case _ => new java.io.PrintWriter(new FileWriter(f, false)) // default is overwrite
    }
    try { op(p) } finally { p.close() }
  }

  def writeLine(in: String, file: String, append: String): Unit = {
    val w = append match {
      case "append" => new BufferedWriter(new FileWriter(file, true))
      case "overwrite" => new BufferedWriter(new FileWriter(file, false))
      case _ => throw new RuntimeException("Specify append or overwrite")
    }
    w.write(in)
    w.close()
  }

  def toASPprogram(
      program: List[String] = Nil,
      generateDirectives: List[String] = Nil,
      generateAtLeast: Int = 1000000000,
      generateAtMost: Int = 1000000000,
      minimizeStatements: List[String] = Nil,
      maximizeStatements: List[String] = Nil,
      constraints: List[List[String]] = Nil,
      show: List[String] = Nil,
      extra: List[String] = Nil,
      writeTo: String): Any = {

    clearFile(writeTo) // clear here, append everywhere else.
    writeToFile(new java.io.File(writeTo), "append")(p => program foreach (p.println))
    val genStatems = (generateDirectives, generateAtLeast, generateAtMost) match {
      case x @ (Nil, _, _) => List()
      case x @ (head :: tail, 1000000000, 1000000000) => for (e <- x._1) yield "{" + e + "}."
      case x @ (head :: tail, lower, 1000000000) => (head :: tail).map(y => "$lower {" + y + "}.\n")
      case x @ (head :: tail, 1000000000, upper) => (head :: tail).map(y => "0 {" + y + "} $upper.\n")
      case x @ (head :: tail, lower, upper) => (head :: tail).map(y => "$lower {" + y + "} $upper.\n")
    }
    writeToFile(new java.io.File(writeTo), "append")(p => genStatems foreach (p.println))
    val minStatement = minimizeStatements match { // This is a single string
      case Nil => ""
      case _ => "#minimize{ " + minimizeStatements.mkString(",") + "}.\n"
    }
    val maxStatement = maximizeStatements match { // This is a single string
      case Nil => ""
      case _ => "#maximize{ " + maximizeStatements.mkString(",") + "}.\n"
    }
    val constrs = constraints match { // This is a list of strings
      case Nil => List("")
      case _ => for (x <- constraints) yield ":- " + x.mkString(",") + ".\n"
    }
    writeLine(minStatement, writeTo, "append")
    writeLine(maxStatement, writeTo, "append")
    writeToFile(new java.io.File(writeTo), "append")(p => constrs foreach (p.println))
    val showDirs = show match {
      case Nil => ""
      case _ => "\n#show.\n" + (show map (x => s"\n#show $x.")).mkString("\n")
    }
    writeLine(showDirs, writeTo, "append")
    writeToFile(new java.io.File(writeTo), "append")(p => extra foreach (p.println))

    writeToFile(new java.io.File(writeTo), "append")(p => showDirs foreach (p.println))
    val debug = scala.io.Source.fromFile(writeTo).mkString
  }

  def growNewRuleTest(clauses: Theory, e: Example, globals: Globals, what: String): Boolean = {

    val targetClass = what

      def solve(program: String): List[AnswerSet] = {
        val f = getTempFile(s"growNewRuleTest-for-$targetClass", ".lp")
        writeToFile(f, "append")(p => List(program) foreach p.println)
        val path = f.getCanonicalPath

        solveASP(Globals.GROW_NEW_RULE_TEST, path)
      }

    val (includeBKfile, failedTestDirective, show) = {
      targetClass match {
        // If we are learning the initiatedAt part of the the theory, then we must start growing
        // a new rule if we have FNs, i.e. no initiatedAt rule in the current hypothesis fires,
        // and fluents are not initiated when they should.
        case "initiatedAt" =>
          if (Globals.glvalues("with-inertia").toBoolean) {
            (globals.INCLUDE_BK(globals.INITIATED_ONLY_INERTIA), globals.FNS_RULES, globals.SHOW_FNS_ARITY_1)
          } else {
            (globals.INCLUDE_BK(globals.BK_INITIATED_ONLY), globals.FNS_RULES, globals.SHOW_FNS_ARITY_1)
          }

        // If we are learning the terminatedAt part of the the theory, then we must start growing
        // a new rule if we have FPs, i.e. no terminatedAt rule in the current hypothesis fires,
        // and fluents are not terminated when they should.
        case "terminatedAt" =>
          (globals.INCLUDE_BK(globals.BK_TERMINATED_ONLY), globals.FPS_RULES, globals.SHOW_FPS_ARITY_1)
        // In this case no theory has been generated yet. We therefore check if the current example
        // satisfies the empty theory with the plain isSAT method. To do that, we use the whole set
        // of EC axioms in the BK. Also, coverage directives (normally fps, tns etc) are coverage
        // constraints here, forcing the SOLVER to try to satisfy them and getting back an UNSAT program in case of failure.
        // Note that we use no #show here.
        case "empty" =>
          // the target is taken from the method's input here.
          (if (targetClass == "initiatedAt") globals.INCLUDE_BK(globals.BK_INITIATED_ONLY)
          else globals.INCLUDE_BK(globals.BK_TERMINATED_ONLY),
            if (targetClass == "initiatedAt") globals.CONSTRAINT_COVER_ALL_POSITIVES
            else globals.CONSTRAINT_EXCLUDE_ALL_NEGATIVES, "") // no #show
      }
    }

    val modes = globals.MODEHS ++ globals.MODEBS
    val t = clauses.map(x => x.withTypePreds(modes).tostring).mkString("\n")

    val ex = e.toASP().mkString("\n")

    val program = ex + includeBKfile + t + failedTestDirective + show
    // Fail if either one of the existing rules
    val failure = (atoms: List[String]) =>
      if (targetClass != "empty")
        targetClass match {
          case "initiatedAt" => atoms.exists(p => p.startsWith("fns"))
          case "terminatedAt" => atoms.exists(p => p.startsWith("fps"))
        }
      else atoms.head == globals.UNSAT // then the example does not satisfy the empty theory. No rules are needed.

    //val timeStart = System.nanoTime()

    val answerSet = solve(program)

    //val timeEnd = System.nanoTime()

    //println(s"growNewRuleTest solving time: ${(timeEnd-timeStart)/1000000000.0}")

    answerSet.nonEmpty match {
      case true =>
        val atoms = answerSet.head.atoms
        if (failure(atoms)) true
        else false
      case _ => false
    }
  }

  /*def growNewRuleTest(clauses: List[Clause], e: Example, globals: Globals): Boolean = {
    // we already have the target with the input (the target parameter).
    // But the one from the input is used only in case of an empty theory. In
    // other cases we get the target class by looking at the rules' heads,
    // just for some extra safety on whether we're indeed learning separately
    // (check out the exceptions thrown below in case we end with a mixture
    // of initiatedAt and terminated rules in the theory.)

    def getTargetClass = {
      val what = clauses.map(x => x.head.predSymbol).toSet
      if(what.size > 1) {
        val msg = s"\nI'm learning both initiated and terminated rules in the same process!"
        throw new RuntimeException(msg)
      }
      if(what.nonEmpty) what.head else "empty"
    }

    val targetClass = getTargetClass

    /*def solve(program: String): List[AnswerSet] = {
      val f = Utils.getTempFile(s"growNewRuleTest-for-$target",".lp")
      Utils.writeToFile(f, "append")(p => List(program) foreach p.println)
      val path = f.getCanonicalPath
      //ASP.solve(task = Core.INFERENCE, aspInputFile = new File(path), jep=jep)
      ASP.solve(task = Globals.GROW_NEW_RULE_TEST, aspInputFile = new File(path))
    }*/

    val (includeBKfile,failedTestDirective,show) = {
      targetClass match {
        // If we are learning the initiatedAt part of the the theory, then we must start growing
        // a new rule if we have FNs, i.e. no initiatedAt rule in the current hypothesis fires,
        // and fluents are not initiated when they should.
        case "initiatedAt" =>
          if (Globals.glvalues("with-inertia").toBoolean) {
            (globals.INCLUDE_BK(globals.INITIATED_ONLY_INERTIA), globals.FNS_RULES,globals.SHOW_FNS_ARITY_1)
          } else {
            (globals.INCLUDE_BK(globals.BK_INITIATED_ONLY), globals.FNS_RULES,globals.SHOW_FNS_ARITY_1)
          }

        // If we are learning the terminatedAt part of the the theory, then we must start growing
        // a new rule if we have FPs, i.e. no terminatedAt rule in the current hypothesis fires,
        // and fluents are not terminated when they should.
        case "terminatedAt" =>
          (globals.INCLUDE_BK(globals.BK_TERMINATED_ONLY), globals.FPS_RULES,globals.SHOW_FPS_ARITY_1)
        // In this case no theory has been generated yet. We therefore check if the current example
        // satisfies the empty theory with the plain isSAT method. To do that, we use the whole set
        // of EC axioms in the BK. Also, coverage directives (normally fps, tns etc) are coverage
        // constraints here, forcing the SOLVER to try to satisfy them and getting back an UNSAT program in case of failure.
        // Note that we use no #show here.
        case "empty" =>
          // the target is taken from the method's input here.
          (if (targetClass=="initiatedAt") globals.INCLUDE_BK(globals.BK_INITIATED_ONLY)
          else globals.INCLUDE_BK(globals.BK_TERMINATED_ONLY),
            if (targetClass=="initiatedAt") globals.CONSTRAINT_COVER_ALL_POSITIVES
            else globals.CONSTRAINT_EXCLUDE_ALL_NEGATIVES,"") // no #show
      }
    }

    val modes = globals.MODEHS++globals.MODEBS
    val t = clauses.map(x => x.withTypePreds(modes).tostring).mkString("\n")

    // Getting exmplWithInertia here does not cause problems (in the initiated case). See comments at CaviarUtils.getDataAsChunks
    val ex = (e.queryAtoms ++ e.observations).map(x => x+".").mkString("\n")

    val program = ex + includeBKfile + t + failedTestDirective + show
    // Fail if either one of the existing rules
    val failure = (atoms: Set[String]) =>
      if (targetClass != "empty")
        targetClass match {
          case "initiatedAt" => atoms.exists(p => p.startsWith("fns"))
          case "terminatedAt" => atoms.exists(p => p.startsWith("fps"))
        }
      else atoms.head == globals.UNSAT // then the example does not satisfy the empty theory. No rules are needed.


    //val timeStart = System.nanoTime()

    val answerSet = ASPSolver.solve(program)

    //val timeEnd = System.nanoTime()

    //println(s"growNewRuleTest solving time: ${(timeEnd-timeStart)/1000000000.0}")

    answerSet.nonEmpty match {
      case true =>
        val atoms = answerSet
        if (failure(atoms)) true
        else false
      case _ => false
    }
  }*/

}

class ASPResultsParser extends ClausalLogicParser {

  def aspResult: Parser[List[String]] = repsep(literal, "") ^^ { case x => for (y <- x) yield y.tostring }

  def parseASP(parser: Parser[Any], expression: String): Option[Any] = {
    parseAll(parser, expression) match {
      case Success(result, _) => Some(result)
      case Failure(msg, _) =>
        println("FAILURE: " + msg); None
      case Error(msg, _) => println("ERROR: " + msg); None
    }
  }

  def parsed(x: Option[Any]): Boolean = x match {
    case Some(y) => true
    case _ => false
  }

  def getResult(x: Option[Any]): Any = x match {
    case Some(y) => y
    case _ => false
  }

}
