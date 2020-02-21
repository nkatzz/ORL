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

import java.io.File
import java.text.DecimalFormat
import java.util.UUID

import orl.app.runutils.RunningOptions
import orl.datahandling.Example
import orl.inference.ASPSolver
import orl.learning.structure.OldStructureLearningFunctions
import orl.learning.weights.UpdateWeights
import orl.logic.{Clause, Literal}

/**
  * Created by nkatz at 11/2/20
  */

class ASPWeightedInference(val rules: Seq[Clause], val exmpl: Example, val inps: RunningOptions) {

  var TPs = 0
  var FPs = 0
  var FNs = 0

  private def minDiff = {

    // I'm using this to keep the weights low, otherwise Clingo may crash (it has crashed with Adam).
    def format(x: Double) = {
      val defaultNumFormat = new DecimalFormat("0.######")
      defaultNumFormat.format(x).toDouble
    }

    val weights = rules.map(x => x.weight).toSet
    if (weights.size == 1) {
      // Then we either have only one rule in the theory, or all rules have the same weight
      // (for instance, when a set of new rules is generated from mistakes of an emtpy theory).
      // Since there is no relative difference between the rules' weights, just set the minDiff var
      // to 0.001, that will set all integer weights to 20, since scaleFactor * initialWeight = (200/0.001) * 0.0001 = 20.
      // The actual value of the integer weights is not important.
      0.001
    } else {
      // else get the smallest pairwise distance between the weights.
      /**
        * Cut some decimals from the weights to keep theie integer values weights low, otherwise
        * Clingo may crash (it has crashed with Adam). This is the error:
        *
        * *** ERROR: (clingo): void Clasp::MinimizeBuilder::prepareLevels(const Clasp::Solver&, Clasp::SumVec&,
        * Clasp::WeightVec&)@651: Value too large for defined data type: MinimizeBuilder: weight too large
        *
        * */
      //rules.map(x => format(x.weight)).toSet.subsets(2).map(pair => math.abs(pair.head - pair.tail.head)).toVector.min
      rules.map(x => x.weight).toSet.subsets(2).map(pair => math.abs(pair.head - pair.tail.head)).toVector.min
    }
  }

  private lazy val scaleFactor = 2000.0 / minDiff
  private lazy val rulesWithintWeights = rules.map(x => (x, x.weight)).map(x => (x._1, math.round(x._2 * scaleFactor)))

  var satisfiedAtoms = Set.empty[String]

  // Store the programs for debugging.
  private var inferenceProgram = ""
  private var scoringProgram = ""
  private var abductionProgram = ""

  /**
    * Generates choice rules and weak constraints from rules and their weights in order to perform weighted
    * inference with Clingo. From each rule i of the form head_i :- body_i passed-in via the 'rules' parameter
    * in the constructor, this method generates the following:
    *
    * 1. A satisfied/1 definition for the rule, of the form head_i :- satisfied(head_i, rule_i).
    *
    * 2. A choice rule for the satisfied/1 atom in the above definition of the form:
    *    {satisfied(head_i, rule_i)} :- typePreds.
    *    where 'typepreds' is a conjunction of type predicates for the variables that appear in
    *    head_i. For instance if head_i = initiatedAt(meet(X,Y),T) then the choice rule is:
    *    {initiatedAt(meet(X,Y),T), rule_i} :- person(X), person(Y), time(T).
    *
    * 3. If the i-th rule has a positive weight, a weak constraint of the form:
    *    :~ not satisfied(head_i, rule_i), body_i, typePreds_i. [w_i, vars_i]
    *    where w_i is the weight of the i-th rule (after scaling all rules' weights to integer values)
    *    and vars_i are the variables that appear in rule i.
    *    If the i-th rule has a negative weight, a weak constraint of the form:
    *    :~ satisfied(head_i, rule_i), body_i, typePreds_i. [w_i, vars_i]
    *
    *    for example, if the i-th rule is
    *
    *    2.567 initiatedAt(meet(X,Y),T) :- happensAt(active(X),T), happensAt(active(Y),T).
    *
    *    and the integer value of its weight is 2000, the weak constraint is
    *
    *    :~ not satisfied(initiatedAt(meet(X,Y),T), rule_i), happensAt(active(X),T), happensAt(active(Y),T), person(X), person(Y), time(T). [2000,X,Y,T]
    *
    */



  private def inferenceProgramUNSAT = transformRulesUNSAT
  private def inferenceProgramSAT = transformRulesSAT

  /**
    *  The i-th rule: head(X) :- body(X).
    *
    *  Transformed:
    *  unsat(head(X),i) :- head(X), not body(X).
    *  head(X) :- body(X), not unsat(X).
    *
    * */
  private def transformRulesUNSAT = {
    val litsToString = (x: Seq[Literal]) => x.map(_.tostring).mkString(",")
    val all = rulesWithintWeights map { case (rule, intWeight) =>
      val unsatAtom = Literal.parse(s"unsatisfied(${rule.head.tostring},${rule.##})")
      val typePreds = getTypePredicates(rule)
      val unsatDefinition = Clause(head = unsatAtom, body = rule.body ++ typePreds :+ rule.head.negated).tostring
      val ruleDefinition = Clause(head = rule.head, body = rule.body ++ typePreds :+ unsatAtom.negated).tostring
      val weakCosntr = s":~ unsatisfied(${rule.head.predSymbol}(F,T),${rule.##}), fluent(F), time(T). [$intWeight,${rule.##},F,T]"
      val ruleIdPred = s"ruleId(${rule.##})."

      // Be very careful with cases like this one.
      // Here, a definition of the form:
      // satisfied(p(X), Id) :- not satisfied(p(X), Id), entity(X), ruleId(Id).
      // can cause groundings of satisfied/2 with irrelevant ruleid's.
      val satDefinition = s"satisfied(${rule.head.predSymbol}(F,T),${rule.##}) :- not unsatisfied(${rule.head.predSymbol}(F,T),${rule.##}),fluent(F),time(T)."
      s"$unsatDefinition\n$ruleDefinition\n$weakCosntr\n$ruleIdPred\n$satDefinition"
    }
    all.mkString("\n")
  }

  /**
    * The i-th rule: head(X) :- body(X).
    *
    * Transformed:
    * head(X) :- satisfied(head(X),i).
    * {satisfied(head(X),i)} :- body(X).
    * :~ satisfied(head(X),i). [w_i]
    *
    * */
  private def transformRulesSAT = {
    val litsToString = (x: Seq[Literal]) => x.map(_.tostring).mkString(",")
    val all = rulesWithintWeights map { case (rule, intWeight) =>
      val typePreds = getTypePredicates(rule)

      val satAtom = Literal.parse(s"satisfied(${rule.head.tostring},${rule.##})")

      val ruleDefinition = s"${rule.head.predSymbol}(F,T) :- satisfied(${rule.head.predSymbol}(F,T),${rule.##}), fluent(F), time(T)."

      val choiceRule = s"{${satAtom.tostring}} :- ${(rule.body ++ typePreds).map(x => x.tostring).mkString(",")}, X0 != X1."

      val weakCosntr = s":~ satisfied(${rule.head.predSymbol}(F,T),${rule.##}), fluent(F), time(T). [${-intWeight},${rule.##},F,T]"

      s"$ruleDefinition\n$choiceRule\n$weakCosntr\n"
    }
    all.mkString("\n")
  }


  def getTypePredicates(rule: Clause): List[Literal] = {
    //rule.getVars.map(x => Literal.parse(s"${x._type}(${x.name})"))
    List(Literal.parse("person(X0)"), Literal.parse("person(X1)"), Literal.parse("time(X2)"))
  }

  /**
    * Updates the weights and the example coverage counts for the rules in the current theory,
    * by counting groundings in the true and the inferred state respectively.
    *
    * We discriminate between top rules and their specializations. Top rules are used to generate
    * an inferred state and their weights and statistics are updated by a direct comparison of their
    * performance in the inferred and the true state.
    *
    * The specializations are not used in the inference process (although they could, but not using them is a
    * way to reduce the cost of the optimization process that solves a WeightedMaxSat problem to generate
    * the inferred state. Fewer rules, implies an easier optimization problem to solve).
    *
    * To update the specializations' weights and coverage counts we use the following strategy:
    *
    * Let head(X) :- body(X) (*) be a parent rule and head(X) :- body(X),q(X) (**) be one of
    * its specializations generated by the addition of q(X).
    *
    * For each substitution [X/v] for which satisfied(head(v),rule_id) is true for (*),
    * if fires(head(v),rule_id') and parent(rule_id',rule_id) are true, we update (**)
    * in the same way we update the parent rule's statistics (they make the same correct/wrong predictions at those points).
    *
    */

  def updateWeightsAndScore(batchCount: Int) = {

      /**
        * Helper method. Generates meta-rules to capture groundings of the rule. Example:
        *
        * Initial rule: p(X) :- q(X). where X is of type 'entity'.
        * Metarule: fires(p(X), rule_12) :- q(X), entity(X).
        */

      def firesMetaRule(rule: Clause) = {
        val typePreds = getTypePredicates(rule)
        val ruleId = s"${rule.##}"
        val bodyWithTypePreds = {
          if (rule.body.nonEmpty) (rule.body ++ typePreds).map(_.tostring).mkString(",")
          else typePreds.map(_.tostring).mkString(",")
        }
        s"fires(${rule.head.tostring}, $ruleId) :- $bodyWithTypePreds."
      }

    val topRules = rules // This includes level-1 specializations for rules with an empty body.
    val _rulesIdMap = topRules.map(x => x.## -> x) toMap
    var rulesIdMap = _rulesIdMap

    val fireMetaRulesForTopRules = _rulesIdMap.map{ case (id, rule) => firesMetaRule(rule) }.mkString("\n")

    /**
      * Adds pairs of ids & specializations to the rulesIdMap and
      * generates, for each specialization i, with id "rule_i" the following:
      *
      * 1. An atom of the form "parent(rule_i,rule_j)", meaning that the rule with id rule_j is the parent rule of rule_i.
      *
      * 2. An atom of the form specialization(rule_i), to indicate that the rule with id rule_i is a specialization.
      *
      * 3. A fires/1 meta-rule for rule_i of the form fires(head_i) :- body_i.
      *
      */
    val specializationDeclarationAtomsAndFireMetaRules = _rulesIdMap.flatMap { case (id, rule) =>
      rule.refinements.map { refinement =>
        val refId = refinement.##
        rulesIdMap = rulesIdMap + (refId -> refinement)
        val parentAtom = s"parent($refId,$id)."
        val specializationAtom = s"specialization($refId)."
        val metaRule = firesMetaRule(refinement)
        s"$parentAtom $specializationAtom\n$metaRule"
      }
    }.mkString("\n")

    val topRulesAtoms = _rulesIdMap.map(x => s"topRule(${x._1}).").mkString(" ")

    /**
      * The empty-bodied top rules do not participate in the inference process.
      * To score them we simply add their actual tp/fp groundings to their counters,
      * just to be able to calculate the information gain over the parent and kick-start the specialization process.
      * To that end, we need an atom of the form emptyBodied(ruleId) for each empty-bodied rule and a fires/1 meta-rule.
      */
    val emptyBodiedRulesInfo = _rulesIdMap.filter(x => x._2.parentClause.body.isEmpty &&
      // Avoid empty parent clauses (when using a hand-crafted theory)
      x._2.parentClause.head.tostring != "").map { case (id, level_1_Specialization) =>

      val parent = level_1_Specialization.parentClause
      val atom = s"emptyBodied(${parent.##})."

      if (!rulesIdMap.keySet.contains(parent.##)) {
        rulesIdMap = rulesIdMap + (parent.## -> parent)
      }

      val metaRule = firesMetaRule(parent)
      s"$atom\n$metaRule"
    }.mkString("\n")

    /**
      * The program that will be passed to the ASP solver to count groundings in the true and the
      * inferred state respectively. It consists of:
      *
      * 1. The data in the current batch
      *
      * 2. The satisfied/1 atoms in the inferred state (have been previousely generated in the inference process
      *    of the makePredictions method) and are stored in the satisfiedAtoms variable of this class.
      *
      * 3. The topRule/1, specialization/1, parent/2 atoms and fires/1 definitions generated previousely in this method.
      *
      * 4. The domain-specific BK (/$entryPath/bk.lp), included in the program via Clingo's #include command.
      *
      * 4. The domain-independent, rule scoring-related BK from BK.ScoreAndUpdateWeightsMetaProgram. #show directives
      *    for this program are included in ScoreAndUpdateWeightsMetaProgram.
      *
      */

    // These are not used
    //def f(x: Clause) = s"${x.head.predSymbol}Rule(${x##})."
    //val initTermAtoms = rules.flatMap { x => x.refinements.map(f) :+ f(x) } mkString(" ")

    val program = {
      val data = exmpl.toASP().mkString(" ")
      val satAtoms = satisfiedAtoms.map(_ + ".").mkString(" ")
      val include = s"""#include "${inps.globals.BK_WHOLE}"."""
      val scoringRules = BK.ScoreAndUpdateWeightsMetaProgram

      s"$data\n$satAtoms\n$topRulesAtoms\n$specializationDeclarationAtomsAndFireMetaRules\n$fireMetaRulesForTopRules\n$emptyBodiedRulesInfo\n\n$include\n$scoringRules"
    }

    scoringProgram = program // Save the program (for debugging purposes)

    val result = ASPSolver.solve(program)

    /**
      * There are atoms with 4 different predicates in the results:
      * 1. resultTopRule/5 atoms, containing example coverage counts for the top rules.
      * 2. resultSpecialization/5 atoms, containing example coverage counts for the specialization rules.
      * 3. total_groundings/1 atoms, returning the total number of groundings for the target predicate in the current data batch.
      * 4. inertia/1 atoms, with holdsAt/2 atoms that are inferred at the last time point of the batch (to roll them over to the next).
      *
      */
    val (totalGroundings, inertiaAtoms, topRuleCounts, specializationCounts) =
      result.foldLeft(0, Vector.empty[Literal], Vector.empty[String], Vector.empty[String]) { (x, y) =>
        if (y.startsWith("total_groundings")) {
          val num = y.split("\\(")(1).split("\\)")(0).toInt
          (num, x._2, x._3, x._4)
        } else if (y.startsWith("inertia")) {
          val parsed = Literal.parse(y)
          val atom = parsed.terms.head.asInstanceOf[Literal]
          (x._1, x._2 :+ atom, x._3, x._4)
        } else if (y.startsWith("resultTopRule")) {
          (x._1, x._2, x._3 :+ y, x._4)
        } else {
          (x._1, x._2, x._3, x._4 :+ y)
        }
      }

    val rulesResults = topRuleCounts ++ specializationCounts

    /**
      * Update weights
      */
    rulesResults foreach { x =>
      val split = x.split(",")
      val ruleId = split(0).split("\\(")(1)
      val actualTrueGroundings = split(1).toInt
      val actualFalseGroundings = split(2).toInt
      val trueInferredAsTrueGroundings = split(3).toInt
      val falseInferredAsTrueGroundings = split(4).split("\\)")(0).toInt
      val allInferredTrue = trueInferredAsTrueGroundings + falseInferredAsTrueGroundings
      val rule = rulesIdMap(ruleId.toInt)
      val mistakes = allInferredTrue - actualTrueGroundings

      rule.weight = UpdateWeights.adaGradUpdate(rule, mistakes, inps)
      //rule.weight = UpdateWeights.adamUpdate(rule, mistakes, inps, batchCount)

      rule.actualGroundings += actualTrueGroundings + actualFalseGroundings

      if (rule.body.isEmpty) { // the empty-bodied rules do not participate in the inference process
        rule.tps += actualTrueGroundings
        rule.fps += actualFalseGroundings
      } else {
        rule.tps += trueInferredAsTrueGroundings
        rule.fps += falseInferredAsTrueGroundings
      }

    }
    (totalGroundings, inertiaAtoms)

  }

  /**
    * This is very experimental, needs a lot of work.
    */
  def abduction() = {
    val program = {

      val data = exmpl.toASP().mkString(" ")
      val rs = inferenceProgramUNSAT
      val abductionBK = BK.abductionMetaProgram
      val include = s"""#include "${inps.globals.BK_WHOLE}"."""
      s"$data\n$rs\n$abductionBK\n$include\n"
    }

    abductionProgram = program
    var options = s"--opt-mode=opt"
    var result = ASPSolver.solve(program, options)

    val abduced = result.map { atom =>
      val lit = Literal.parse(atom)
      val modeCounter = lit.terms.head
      val proxyAtom = lit.terms.tail.head.asInstanceOf[Literal]
      val actualAtom = lit.terms.tail.tail
      // the head is of the form initiatedAt_proxy, terminatedAt_proxy here.
      val oldPredSymbol = proxyAtom.predSymbol
      val newPredSymbol = oldPredSymbol.split("_")(0)
      val newAtom = Literal(predSymbol = newPredSymbol, terms = proxyAtom.terms, isNAF = proxyAtom.isNAF)

      val newMatchesModeAtom = lit.replace(proxyAtom, newAtom)

      newMatchesModeAtom.tostring
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

    val aspFile: File = getTempFile("aspinput", ".lp")

      def toMapASP(e: Example) = Map("annotation" -> e.queryAtoms.map(x => s"example($x)."), "narrative" -> e.observations.map(x => x + "."))

    val (_, varKernel) = OldStructureLearningFunctions.generateKernel(abduced, examples = toMapASP(exmpl), aspInputFile = aspFile, bkFile = inps.globals.BK_WHOLE_EC, globals = inps.globals)

    val bottomTheory = rules flatMap (x => x.supportSet)

    val goodKernelRules = varKernel.filter(newBottomRule => !bottomTheory.exists(supportRule => newBottomRule.thetaSubsumes(supportRule)))

    goodKernelRules

    val bcs = goodKernelRules

    bcs map { x =>
      val c = Clause(head = x.head, body = List())
      c.addToSupport(x)
      c
    }
  }

  /**
    * Performs inference with the weighted rules to assess the performance of the current theory.
    *
    */
  def performInference(metaProgram: String = "UNSAT") = {

    val idsMap = rules.flatMap(x => List(x)++x.refinements).map(x =>  x.## -> x).toMap

    val program = {
      // The only reason we add the examples here (instead of having the observations only)
      // is to get the last time point in the batch (see the time(T) :- example(holdsAt(_,T)). in the bk)
      // This is necessary in order to not miss positives in the last time point.
      val data = exmpl.toASP().mkString(" ")
      val rs = if (metaProgram == "UNSAT") inferenceProgramUNSAT else inferenceProgramSAT
      val include = s"""#include "${inps.globals.BK_WHOLE}"."""

      val shows = s"#show holdsAt/2.\n#show satisfied/2."
      s"$data\n$rs\n$include\n$shows"
    }

    inferenceProgram = program // Save the program (for debugging purposes)

    //var optCost = 1000
    //var options = s"--opt-mode=enum,$optCost"
    //var result = ASPSolver.solve(program) // No use of options, just return all models.

    //val theory = rules.map(x => s"${x.weight} ${x.tostring} (TPs: ${x.tps}, FPs: ${x.fps})").mkString("\n")
    //println("THEORY:\n"+theory)

    // --opt-strategy=usc helps to boost performance if a large number of heads may be inferred.
    var options = s"--opt-mode=opt --opt-strategy=usc" //--opt-strategy=usc
    var result = ASPSolver.solve(program, options)

    /*val smallIncrement = 10000
    val bigIncrement = 10000000
    var countTries = 0
    var solutionFound = false
    if (result.nonEmpty) {
      if (result.head == "UNSATISFIABLE") {
        while (!solutionFound) {
          countTries += 1
          if (countTries <= 10) optCost += smallIncrement else optCost += bigIncrement
          println(s"Trying $options")
          options = s"--opt-mode=enum,$optCost"
          result = ASPSolver.solve(program, options)
          if (result.head != "UNSATISFIABLE") solutionFound = true
        }
      }
    }*/

    val (satAtoms, inferredAtoms) = result.foldLeft(Set.empty[String], Set.empty[String]) { (x, y) =>
      if (y.startsWith("satisfied")) (x._1 + y, x._2) else (x._1, x._2 + y)
    }

    satisfiedAtoms = satAtoms

    //println(satAtoms)
    //println(inferredAtoms)

    val trueState = exmpl.queryAtoms.toSet
    val inferredState = inferredAtoms

    val tps = trueState.intersect(inferredState).filter(x => !x.startsWith("test"))
    val fps = inferredState.diff(trueState).filter(x => !x.startsWith("test"))
    val fns = trueState.diff(inferredState).filter(x => !x.startsWith("test"))

    TPs = tps.size
    FPs = fps.size
    FNs = fns.size

    (satAtoms, inferredAtoms) // This is returned for debugging purposes
  }

}